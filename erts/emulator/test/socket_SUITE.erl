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


-include("socket_test_evaluator.hrl").

%% Internal exports
%% -export([]).

%% -record(ev, {name :: string(),
%%              pid  :: pid(),
%%              mref :: reference()}).
%% -type ev() :: #ev{}.
-define(MKEV(N,P,R), #ev{name = N, pid = P, mref = R}).
%% -type initial_evaluator_state() :: map().
%% -type evaluator_state() :: map().
%% -type command_fun() :: 
%%         fun((State :: evaluator_state()) -> ok) |
%%         fun((State :: evaluator_state()) -> {ok, evaluator_state()}) |
%%         fun((State :: evaluator_state()) -> {error, term()}).

%% -type command() :: #{desc  := string(),
%%                      cmd   := command_fun()}.


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
%%     [snr_send_and_recv_chunks].


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
                           ok;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



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
                           ok = socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



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
                           Tester = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "await connection",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: ~n   ~p", [Sock]),
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
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
                           ?SEV_ANNOUNCE_READY(Tester, recv_req),
                           ok
                   end},
         #{desc => "await continue (with send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_reply)
                   end},
         #{desc => "send reply",
           cmd  => fun(#{csock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REP)
                   end},
         #{desc => "announce ready (send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           socket:connect(Sock, SSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (send request)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_req)
                   end},
         #{desc => "send request (to server)",
           cmd  => fun(#{sock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REQ)
                   end},
         #{desc => "announce ready (send request)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_req),
                           ok
                   end},
         #{desc => "await recv reply (from server)",
           cmd  => fun(#{sock := Sock, recv := Recv}) ->
                           {ok, ?BASIC_REP} = Recv(Sock),
                           ok
                   end},
         #{desc => "announce ready (recv reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_port => Port}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect)
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept)
                   end},
         #{desc => "order client to continue (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_req),
                           ok
                   end},
         #{desc => "await client ready (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_req)
                   end},
         #{desc => "await server ready (request recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_req)
                   end},
         #{desc => "order server to continue (with send reply)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_reply),
                           ok
                   end},
         #{desc => "await server ready (with reply sent)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_reply)
                   end},
         #{desc => "await client ready (reply recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_reply)
                   end},


         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client, State),
                           {ok, State1}
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           State1 = maps:remove(server, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    Server = ?SEV_START("server", ServerSeq, InitState),

    i("start client evaluator"),
    Client = ?SEV_START("client", ClientSeq, InitState),
    i("await evaluator(s)"),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



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
    Tester1 = ?SEV_START("tcp-tester", Seq, InitState1),
    i("await tcp evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester1]),

    i("start udp (dgram) socket"),
    InitState2 = #{domain => inet, type => dgram, protocol => udp},
    Tester2 = ?SEV_START("udp-tester", Seq, InitState2),
    i("await udp evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester2]).


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
                           {Tester, Sock} = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, not_owner),
                           ok
                   end},
         #{desc => "await continue (owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, owner)
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
                           ?SEV_ANNOUNCE_READY(Tester, owner),
                           ok

                   end},
         
         %% *** Termination ***
         #{desc => "await termination",
           cmd  => fun(#{tester := Tester} = State) ->
                           ?SEV_AWAIT_TERMINATE(Tester, tester),
                           State1 = maps:remove(tester, State),
                           State2 = maps:remove(sock, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Client, Sock),
                           ok
                   end},
         #{desc => "await (client) ready (not owner)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, not_owner)
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
                           ?SEV_ANNOUNCE_CONTINUE(Client, owner),
                           ok
                   end},
         #{desc => "await (client) ready (2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, owner),
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
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           {ok, maps:remove(client, State)}
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start tcp (stream) client evaluator"),
    ClientInitState1 = #{},
    Client1 = ?SEV_START("tcp-client", ClientSeq, ClientInitState1),

    i("start tcp (stream) tester evaluator"),
    TesterInitState1 = #{domain   => inet, 
                         type     => stream, 
                         protocol => tcp,
                         client   => Client1#ev.pid},
    Tester1 = ?SEV_START("tcp-tester", TesterSeq, TesterInitState1),

    i("await tcp evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Tester1, Client1]),

    i("start udp (dgram) client evaluator"),
    ClientInitState2 = #{},
    Client2 = ?SEV_START("udp-client", ClientSeq, ClientInitState2),

    i("start udp (dgram) tester evaluator"),
    TesterInitState2 = #{domain   => inet, 
                         type     => dgram, 
                         protocol => udp,
                         client   => Client2#ev.pid},
    Tester2 = ?SEV_START("udp-tester", TesterSeq, TesterInitState2),

    i("await udp evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Tester2, Client2]).



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
                           Tester = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Server),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server, local_sa := LSA} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Server, server, init),
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
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await (server) down",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
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
         ?SEV_FINISH_NORMAL
        ],

    i("create server evaluator"),
    ServerInitState = InitState,
    Server          = ?SEV_START("server", ServerSeq, ServerInitState),

    i("create tester evaluator"),
    TesterInitState = InitState#{server => Server#ev.pid},
    Tester          = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Tester]).


api_to_connect_tcp_await_timeout(Socks, To, ServerSA) ->
    api_to_connect_tcp_await_timeout(Socks, To, ServerSA, 1).

api_to_connect_tcp_await_timeout([], _To, _ServerSA, _ID) ->
    ?FAIL(unexpected_success);
api_to_connect_tcp_await_timeout([Sock|Socks], To, ServerSA, ID) ->
    ?SEV_IPRINT("~w: try connect", [ID]),
    Start = t(),
    case socket:connect(Sock, ServerSA, To) of
        {error, timeout} ->
            ?SEV_IPRINT("expected timeout (~w)", [ID]),
            Stop  = t(),
            TDiff = tdiff(Start, Stop),
            if
                (TDiff >= To) ->
                    ok;
                true ->
                    {error, {unexpected_timeout, TDiff, To}}
            end;
        {error, econnreset = Reason} ->
            ?SEV_IPRINT("failed connecting: ~p - giving up", [Reason]),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("failed connecting: ~p", [Reason]),
            ?FAIL({connect, Reason});
        ok ->
            ?SEV_IPRINT("unexpected success (~w) - try next", [ID]),
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
         ?SEV_FINISH_NORMAL
        ],

    i("create tester evaluator"),
    Tester = ?SEV_START("tester", TesterSeq, InitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester]).



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
                           Tester = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init, LSock),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   ?SEV_EPRINT("Unexpected accept success: "
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
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_TERMINATE(Tester, tester),
                           ok
                   end},
         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, LSock} = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test part ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
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
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor ready (init)",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_acceptor, init),
                           {ok, State#{lsock => Sock}}
                   end},

         %% Start sec-acceptor-1
         #{desc => "start sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 ready (init)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, init)
                   end},

         %% Start sec-acceptor-2
         #{desc => "start sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 ready (init)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, init)
                   end},

         %% Activate the acceptor(s)
         #{desc => "active prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},

         %% Await acceptor(s) completions
         #{desc => "await prim-acceptor ready (accept)",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_acceptor, accept)
                   end},
         #{desc => "await sec-acceptor 1 ready (accept)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, accept)
                   end},
         #{desc => "await sec-acceptor 2 ready (accept)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, accept)
                   end},

         %% Terminate
         #{desc => "order prim-acceptor to terminate",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor termination",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(prim_acceptor, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 1 to terminate",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 termination",
           cmd  => fun(#{sec_acceptor1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 2 to terminate",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 termination",
           cmd  => fun(#{sec_acceptor2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         
         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("create prim-acceptor evaluator"),
    PrimAInitState = InitState,
    PrimAcceptor = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAInitState),

    i("create sec-acceptor 1 evaluator"),
    SecAInitState1 = maps:remove(domain, InitState),
    SecAcceptor1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAInitState1),
    
    i("create sec-acceptor 2 evaluator"),
    SecAInitState2 = SecAInitState1,
    SecAcceptor2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAInitState2),

    i("create tester evaluator"),
    TesterInitState = #{prim_acceptor => PrimAcceptor#ev.pid,
                        sec_acceptor1 => SecAcceptor1#ev.pid,
                        sec_acceptor2 => SecAcceptor2#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([PrimAcceptor, SecAcceptor1, SecAcceptor2, Tester]).



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
                           Tester = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept and recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept_recv)
                   end},
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
         #{desc => "announce ready (recv timeout success)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_recv),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester      => Tester,
                                       server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
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
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (with connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "connect",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           sock_connect(Sock, SSA),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},

         %% *** Activate server ***
         #{desc => "start server",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_START(Server),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Server, server, init),
                           {ok, State#{server_port => Port}}
                   end},
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept_recv),
                           ok
                   end},

         %% *** Activate client ***
         #{desc => "start client",
           cmd  => fun(#{client := Client, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Client, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, init)
                   end},

         %% *** The actual test ***
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await server ready (accept/recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept_recv)
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Client) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Server) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   State2 = maps:remove(server_port, State1),
                                   {ok, State2};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    
    i("start server evaluator"),
    ServerInitState = InitState,
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid, 
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



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
         ?SEV_FINISH_NORMAL
        ],

    i("start tester evaluator"),
    Tester = ?SEV_START("tester", TesterSeq, InitState),
    
    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester]).



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
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% *** The actual test ***
         %% We *intentially* leave the socket "as is", no explicit close
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify owner as controlling-process",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, Pid} ->
                                   ok;
                               {ok, Other} ->
                                   {error, {unexpected_owner, Other}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% The reason we get closed, is that as long as there is a ref to 
         %% the resource (socket), then it will not be garbage collected.
         #{desc => "verify no socket (closed)",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, OtherPid} ->
                                   {error, {unexpected_success, Pid, OtherPid}};
                               {error, closed} ->
                                   ok;
                               {error, Reason} ->
                                   ?SEV_IPRINT("expected failure: ~p", [Reason]),
                                   {error, {unexpected_failure, Reason}}
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).



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
    %% This (acceptor) is the server that accepts connections.
    %% But it is also suppose to close the connection socket, 
    %% and trigger the read failure (=closed) for the handler process.
    AcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
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
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
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
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, {H1, H2, H3}} ->
                                   {ok, State#{handler1 => H1,
                                               handler2 => H2,
                                               handler3 => H3}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("connection accepted"),
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},
         #{desc => "transfer connection to handler 1",
           cmd  => fun(#{handler1 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 2",
           cmd  => fun(#{handler2 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 3",
           cmd  => fun(#{handler3 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close),
                           ok
                   end},
         #{desc => "close the connection socket",
           cmd  => fun(#{csock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(csock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{lsock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(lsock, State),
                                   State2 = maps:remove(lport, State1),
                                   {ok, State2};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is to perform the recv for which we are testing the reponse
    HandlerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Acceptor} = ?SEV_AWAIT_START(),
                           {ok, State#{tester  => Tester, 
                                      acceptor => Acceptor}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Acceptor} = _State) ->
                           _MRef = erlang:monitor(process, Acceptor),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (transfer)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Pid, acceptor, transfer) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (transfer)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, transfer),
                           ok
                   end},
         #{desc => "attempt recv (=> closed)",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           case Recv(Sock) of
                               {ok, _Data} ->
                                   ?SEV_EPRINT("Unexpected data received"),
                                   {error, unexpected_success};
                               {error, closed} ->
                                   ?SEV_IPRINT("received expected 'closed' result"),
                                   State1 = maps:remove(sock, State),
                                   {ok, State1};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Unexpected read faulure: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is basically just to create the connection.
    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
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
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester, local_sa := LSA} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, connect) of
                               {ok, Port} ->
                                   ServerSA = LSA#{port => Port},
                                   {ok, State#{server_sa => ServerSA}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := ServerSA}) ->
                           socket:connect(Sock, ServerSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
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
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await acceptor ready (init)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% Start the handler(s)
         #{desc => "order handler 1 start",
           cmd  => fun(#{acceptor := Acceptor, handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler1, init)
                   end},
         #{desc => "order handler 2 start",
           cmd  => fun(#{acceptor := Acceptor, handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler2, init)
                   end},
         #{desc => "order handler 3 start",
           cmd  => fun(#{acceptor := Acceptor, handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler3, init)
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% The actual test
         #{desc => "order acceptor to continue (accept)",
           cmd  => fun(#{acceptor := Pid, 
                         handler1 := H1, 
                         handler2 := H2, 
                         handler3 := H3} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, {H1, H2, H3}),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (connect)",
           cmd  => fun(#{client := Pid, lport := Port} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect, Port),
                           ok
                   end},
         #{desc => "await acceptor ready (accept)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, accept)
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, connect)
                   end},
         #{desc => "await handler 1 ready (transfer)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, transfer)
                   end},
         #{desc => "await handler 2 ready (transfer)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, transfer)
                   end},
         #{desc => "await handler 3 ready (transfer)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, transfer)
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order acceptor to continue (close connection socket)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await acceptor ready (close)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, close)
                   end},
         #{desc => "await handler 1 ready (recv closed)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, recv_closed)
                   end},
         #{desc => "await handler 2 ready (recv closed)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, recv_closed)
                   end},
         #{desc => "await handler 3 ready (recv closed)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, recv_closed)
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order acceptor to terminate",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await acceptor termination",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(acceptor, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start acceptor evaluator"),
    AccInitState = InitState,
    Acceptor = ?SEV_START("acceptor", AcceptorSeq, AccInitState),

    i("start handler 1 evaluator"),
    HandlerInitState = #{recv => maps:get(recv, InitState)},
    Handler1 = ?SEV_START("handler-1", HandlerSeq, HandlerInitState),

    i("start handler 2 evaluator"),
    Handler2 = ?SEV_START("handler-2", HandlerSeq, HandlerInitState),

    i("start handler 3 evaluator"),
    Handler3 = ?SEV_START("handler-3", HandlerSeq, HandlerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{acceptor => Acceptor#ev.pid,
                        handler1 => Handler1#ev.pid,
                        handler2 => Handler2#ev.pid,
                        handler3 => Handler3#ev.pid,
                        client   => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Acceptor, 
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
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "open socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           Sock = sock_open(Domain, dgram, udp),
                           SA   = sock_sockname(Sock),
                           {ok, State#{sock => Sock, sa => SA}}
                   end},
         #{desc => "bind socket",
           cmd  => fun(#{sock := Sock, local_sa := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv, with timeout)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "receive, with timeout",
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
         #{desc => "announce ready (recv, with timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
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
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, terminate) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                           
                   end},
         #{desc => "receive",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           case Recv(Sock, infinity) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' ready (init)",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_server, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary server 1
         #{desc => "order 'secondary server 1' start",
           cmd  => fun(#{sec_server1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 1' ready (init)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server1, init)
                   end},

         %% Start the secondary server 2
         #{desc => "order 'secondary server 2' start",
           cmd  => fun(#{sec_server2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 2' ready (init)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server2, init)
                   end},

         %% Start the secondary server 3
         #{desc => "order 'secondary server 3' start",
           cmd  => fun(#{sec_server3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 3' ready (init)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary server 1' to continue (recv)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 2' to continue (recv)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 3' to continue (recv)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary server' to continue (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary server' ready (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, recv)
                   end},
         #{desc => "order 'primary server' to continue (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary server' ready (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, close)
                   end},
         #{desc => "await 'secondary server 1' ready (closed)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server1, recv_closed)
                   end},
         #{desc => "await 'secondary server 2' ready (closed)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server2, recv_closed)
                   end},
         #{desc => "await 'secondary server 3' ready (closed)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server3, recv_closed)
                   end},

         %% Terminations
         #{desc => "order 'secondary server 3' to terminate",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 3' termination",
           cmd  => fun(#{sec_server3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 2' to terminate",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 2' termination",
           cmd  => fun(#{sec_server2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 1' to terminate",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 1' termination",
           cmd  => fun(#{sec_server1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary server' to terminate",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' termination",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_server, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    

    i("start 'primary server' evaluator"),
    PrimSrvInitState = InitState,
    PrimServer = ?SEV_START("prim-server", PrimServerSeq, PrimSrvInitState),

    i("start 'secondary server 1' evaluator"),
    SecSrvInitState = #{recv => maps:get(recv, InitState)},
    SecServer1 = ?SEV_START("sec-server-1", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 2' evaluator"),
    SecServer2 = ?SEV_START("sec-server-2", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 3' evaluator"),
    SecServer3 = ?SEV_START("sec-server-3", SecServerSeq, SecSrvInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_server => PrimServer#ev.pid,
                        sec_server1  => SecServer1#ev.pid,
                        sec_server2  => SecServer2#ev.pid,
                        sec_server3  => SecServer3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimServer, 
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
    %% Each connection are handled by handler processes.
    %% These are created (on the fly) and handled internally 
    %% by the server!
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
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
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test (we expect three connections)
         #{desc => "await continue (accept all three connections)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept 1",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(1, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock1   => Sock,
                                               handler1 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handle 1 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1} = _State) ->
                           ?SEV_AWAIT_READY(Handler1, handler1, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "accept 2",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(2, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock2   => Sock,
                                               handler2 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handle 2 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2} = _State) ->
                           ?SEV_AWAIT_READY(Handler2, handler2, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1}])
                   end},
         #{desc => "accept 3",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(3, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock3   => Sock,
                                               handler3 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handle 3 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2, 
                         handler3 := Handler3} = _State) ->
                           ?SEV_AWAIT_READY(Handler3, handler3, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1},
                                             {handler2, Handler2}])
                   end},
         #{desc => "announce ready (accept all three connections)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler 1 to receive",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 2 to receive",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 3 to receive",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler 1 (recv)",
           cmd  => fun(#{tester := Tester, handler1 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler1, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 2 (recv)",
           cmd  => fun(#{tester := Tester, handler2 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler2, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 3 (recv)",
           cmd  => fun(#{tester := Tester, handler3 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler3, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed from all handlers)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},
         
         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           %% Pid ! {terminate, self(), ok},
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock1,   State),
                           State2 = maps:remove(handler1, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock2,   State),
                           State2 = maps:remove(handler2, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock3,   State),
                           State2 = maps:remove(handler3, State1),
                           {ok, State2}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   {ok, maps:remove(lsock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, {NodeID, ServerSA}} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       node_id   => NodeID, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(#{host := Host, node_id := NodeID} = State) ->
                           case start_node(Host, l2a(f("client_~w", [NodeID]))) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("client node ~p started", [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason, _} ->
                                   {error, Reason}
                           end
                   end},
         #{desc => "monitor client node 1",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = sc_rc_tcp_client_start(Node),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient := Client, server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client process ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, connect, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (connected)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to close",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, close),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, close, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rclient, Client}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "kill remote client",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await remote client termination",
           cmd  => fun(#{rclient := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   State1 = maps:remove(node_id, State),
                                   State2 = maps:remove(node,    State1),
                                   {ok, State2}
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 1",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 2",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 3",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client(s)
         #{desc => "order client 1 start",
           cmd  => fun(#{client1   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {1, ServerSA}),
                           ok
                   end},
         #{desc => "await client 1 ready (init)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "order client 2 start",
           cmd  => fun(#{client2   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {2, ServerSA}),
                           ok
                   end},
         #{desc => "await client 2 ready (init)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "order client 3 start",
           cmd  => fun(#{client3   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {3, ServerSA}),
                           ok
                   end},
         #{desc => "await client 3 ready (init)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},

         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (connect)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 1 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, connect, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (connect)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 2 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (connect)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 3 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client3, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (accept from all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order server continue (recv for all connections)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (close)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 1 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, close, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (close)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 2 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (close)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 3 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client1, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (close for all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_closed,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},

         %% Terminations
         #{desc => "order client 1 to terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 1 termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 2 to terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 2 termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 3 to terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 3 termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client3, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = InitState,
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client1 = ?SEV_START("client-1", ClientSeq, ClientInitState),
    Client2 = ?SEV_START("client-2", ClientSeq, ClientInitState),
    Client3 = ?SEV_START("client-3", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server  => Server#ev.pid,
                        client1 => Client1#ev.pid,
                        client2 => Client2#ev.pid,
                        client3 => Client3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server,
                            Client1, Client2, Client3,
                            Tester]).


start_node(Host, NodeName) ->
    UniqueNodeName = f("~w_~w", [NodeName, erlang:unique_integer([positive])]),
    case do_start_node(Host, UniqueNodeName) of
        {ok, _} = OK ->
            OK;
        {error, Reason, _} ->
            {error, Reason}
    end.

do_start_node(Host, NodeName) when is_list(NodeName) ->
    do_start_node(Host, list_to_atom(NodeName));
do_start_node(Host, NodeName) when is_atom(NodeName) ->
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
    sc_rc_tcp_client_announce_ready(Parent, init),
    sc_rc_tcp_client_await_continue(Parent, connect),
    sc_rc_tcp_client_connect(Sock, ServerSA),
    sc_rc_tcp_client_announce_ready(Parent, connect),
    sc_rc_tcp_client_await_continue(Parent, close),
    sc_rc_tcp_client_close(Sock),
    sc_rc_tcp_client_announce_ready(Parent, close),
    Reason = sc_rc_tcp_client_await_terminate(Parent),
    exit(Reason).

sc_rc_tcp_client_init(Parent, GL) ->
    i("sc_rc_tcp_client_init -> entry"),
    _MRef = erlang:monitor(process, Parent),
    group_leader(self(), GL),
    ok.

sc_rc_tcp_client_await_start(Parent) ->
    i("sc_rc_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

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

sc_rc_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

sc_rc_tcp_client_await_continue(Parent, Slogan) ->
    i("sc_rc_tcp_client_await_continue -> entry"),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

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
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


%% The handlers run on the same node as the server (the local node).

sc_rc_tcp_handler_start(ID, Recv, Sock) ->
    Self     = self(),
    Fun      = fun() -> sc_rc_tcp_handler(ID, Self, Recv, Sock) end,
    {Pid, _} = erlang:spawn_monitor(Fun),
    Pid.

sc_rc_tcp_handler(ID, Parent, Recv, Sock) ->
    sc_rc_tcp_handler_init(ID, Parent),
    sc_rc_tcp_handler_await(Parent, recv),
    RecvRes = sc_rc_tcp_handler_recv(Recv, Sock),
    sc_rc_tcp_handler_announce_ready(Parent, recv, RecvRes),
    Reason = sc_rc_tcp_handler_await(Parent, terminate),
    exit(Reason).

sc_rc_tcp_handler_init(ID, Parent) ->
    put(sname, f("handler-~w", [ID])),
    _MRef = erlang:monitor(process, Parent),
    ?SEV_IPRINT("started"),
    ?SEV_ANNOUNCE_READY(Parent, init),
    ok.

sc_rc_tcp_handler_await(Parent, terminate) ->
    ?SEV_IPRINT("await terminate"),
    ?SEV_AWAIT_TERMINATE(Parent, tester);
sc_rc_tcp_handler_await(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

sc_rc_tcp_handler_recv(Recv, Sock) ->
    ?SEV_IPRINT("recv"),
    try Recv(Sock) of
        {error, closed} ->
            ok;
        {ok, _} ->
            ?SEV_IPRINT("unexpected success"),
            {error, unexpected_success};
        {error, Reason} = ERROR ->
            ?SEV_IPRINT("receive error: "
               "~n   ~p", [Reason]),
            ERROR
    catch
        C:E:S ->
            ?SEV_IPRINT("receive failure: "
               "~n   Class: ~p"
               "~n   Error: ~p"
               "~n   Stack: ~p", [C, E, S]),
            {error, {recv, C, E, S}}
    end.
    
sc_rc_tcp_handler_announce_ready(Parent, Slogan, Result) ->
    ?SEV_IPRINT("announce ready"),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result),
    ok.


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
                           Tester = ?SEV_AWAIT_START(),
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
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await connection",
           cmd  => fun(#{sock := Sock, timeout := Timeout} = _State) ->
                           case socket:accept(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, Sock} ->
                                   ?SEV_EPRINT("unexpected success"),
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_timeout),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
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
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         % Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                    end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init)
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:accept(Sock) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_closed)
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
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
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (init)",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_acc, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary acceptor 1
         #{desc => "order 'secondary acceptor 1' start",
           cmd  => fun(#{sec_acc1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' ready (init)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, init)
                   end},

         %% Start the secondary acceptor 2
         #{desc => "order 'secondary acceptor 2' start",
           cmd  => fun(#{sec_acc2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' ready (init)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, init)
                   end},

         %% Start the secondary acceptor 3
         #{desc => "order 'secondary acceptor 3' start",
           cmd  => fun(#{sec_acc3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' ready (init)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary acceptor 1' to continue (accept)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 2' to continue (accept)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 3' to continue (accept)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary acceptor' to continue",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (accept timeout)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, accept_timeout)
                   end},
         #{desc => "order 'primary acceptor' to continue (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, close)
                   end},
         #{desc => "await 'secondary acceptor 1' ready (accept closed)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 2' ready (accept closed)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 3' ready (accept closed)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, accept_closed)
                   end},
         

         %% Terminations
         #{desc => "order 'secondary acceptor 3' to terminate",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' termination",
           cmd  => fun(#{sec_acc3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 2' to terminate",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' termination",
           cmd  => fun(#{sec_acc2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 1' to terminate",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' termination",
           cmd  => fun(#{sec_acc1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary acceptor' to terminate",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' termination",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_acc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    i("start 'primary acceptor' evaluator"),
    PrimAccInitState = InitState,
    PrimAcc = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAccInitState),

    i("start 'secondary acceptor 1' evaluator"),
    SecAccInitState = #{},
    SecAcc1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 2' evaluator"),
    SecAcc2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 3' evaluator"),
    SecAcc3 = ?SEV_START("sec-acceptor-3", SecAcceptorSeq, SecAccInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_acc => PrimAcc#ev.pid,
                        sec_acc1 => SecAcc1#ev.pid,
                        sec_acc2 => SecAcc2#ev.pid,
                        sec_acc3 => SecAcc3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimAcc, SecAcc1, SecAcc2, SecAcc3, Tester]).



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

l2a(S) when is_list(S) ->
    list_to_atom(S).

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

