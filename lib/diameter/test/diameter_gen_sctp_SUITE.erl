%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% Some gen_sctp-specific tests demonstrating problems that were
%% encountered during diameter development but have nothing
%% specifically to do with diameter. At least one of them can cause
%% diameter_transport_SUITE testcases to fail.
%%

-module(diameter_gen_sctp_SUITE).

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([send_not_from_controlling_process/1,
         send_from_multiple_clients/1, send_from_multiple_clients/0,
         receive_what_was_sent/1]).

-include_lib("kernel/include/inet_sctp.hrl").

%% Message from gen_sctp are of this form.
-define(SCTP(Sock, Data), {sctp, Sock, _, _, Data}).

%% Open sockets on the loopback address.
-define(ADDR, {127,0,0,1}).

%% Snooze, nap, siesta.
-define(SLEEP(T), receive after T -> ok end).

%% An indescribably long number of milliseconds after which everthing
%% that should have happened has.
-define(FOREVER, 2000).

%% The first byte in each message we send as a simple guard against
%% not receiving what was sent.
-define(MAGIC, 42).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [send_not_from_controlling_process,
     send_from_multiple_clients,
     receive_what_was_sent].

init_per_suite(Config) ->
    case gen_sctp:open() of
        {ok, Sock} ->
            gen_sctp:close(Sock),
            Config;
        {error, E} when E == eprotonosupport;
                        E == esocktnosupport ->
            {skip, no_sctp}
    end.

end_per_suite(_Config) ->
    ok.

%% ===========================================================================

%% send_not_from_controlling_process/1
%%
%% This testcase failing shows gen_sctp:send/4 hanging when called
%% outside the controlling process of the socket in question.

send_not_from_controlling_process(_) ->
    Pids = send_not_from_controlling_process(),
    ?SLEEP(?FOREVER),
    try
        [] = [{P,I} || P <- Pids, I <- [process_info(P)], I /= undefined]
    after
        lists:foreach(fun(P) -> exit(P, kill) end, Pids)
    end.

%% send_not_from_controlling_process/0
%%
%% Returns the pids of three spawned processes: a listening process, a
%% connecting process and a sending process.
%%
%% The expected behaviour is that all three processes exit:
%%
%% - The listening process exits upon receiving an SCTP message
%%   sent by the sending process.
%% - The connecting process exits upon listening process exit.
%% - The sending process exits upon gen_sctp:send/4 return.
%%
%% The observed behaviour is that all three processes remain alive
%% indefinitely:
%%
%% - The listening process never receives the SCTP message sent
%%   by the sending process.
%% - The connecting process has an inet_reply message in its mailbox
%%   as a consequence of the call to gen_sctp:send/4 call from the
%%   sending process.
%% - The call to gen_sctp:send/4 in the sending process doesn't return,
%%   hanging in prim_inet:getopts/2.

send_not_from_controlling_process() ->
    FPid = self(),
    {L, MRef} = spawn_monitor(fun() -> listen(FPid) end),
    receive
        {?MODULE, C, S} ->
            demonitor(MRef, [flush]),
            [L,C,S];
        {'DOWN', MRef, process, _, _} = T ->
            error(T)
    end.

%% listen/1

listen(FPid) ->
    {ok, Sock} = open(),
    ok = gen_sctp:listen(Sock, true),
    {ok, PortNr} = inet:port(Sock),
    LPid = self(),
    spawn(fun() -> connect1(PortNr, FPid, LPid) end), %% connecting process
    Id = assoc(Sock),
    recv(Sock, Id).

%% connect1/3

connect1(PortNr, FPid, LPid) ->
    {ok, Sock} = open(),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    Id = assoc(Sock),
    FPid ! {?MODULE,
            self(),
            spawn(fun() -> send(Sock, Id) end)}, %% sending process
    MRef = monitor(process, LPid),
    down(MRef).  %% Waits with this as current_function.

%% down/1

down(MRef) ->
    receive {'DOWN', MRef, process, _, Reason} -> Reason end.

%% send/2

send(Sock, Id) ->
    ok = gen_sctp:send(Sock, Id, 0, <<0:32>>).

%% ===========================================================================

%% send_from_multiple_clients/0
%%
%% Demonstrates sluggish delivery of messages.

send_from_multiple_clients() ->
    [{timetrap, {seconds, 60}}].

send_from_multiple_clients(_) ->
    {S, Rs} = T = send_from_multiple_clients(8, 1024),
    Max = ?FOREVER*1000,
    {false, [], _} = {Max < S,
                      Rs -- [OI || {O,_} = OI <- Rs, is_integer(O)],
                      T}.

%% send_from_multiple_clients/2
%%
%% Opens a listening socket and then spawns a specified number of
%% processes, each of which connects to the listening socket. Each
%% connecting process then sends a message, whose size in bytes is
%% passed as an argument, the listening process sends a reply
%% containing the time at which the message was received, and the
%% connecting process then exits upon reception of this reply.
%%
%% Returns the elapsed time for all connecting process to exit
%% together with a list of exit reasons for the connecting processes.
%% In the successful case a connecting process exits with the
%% outbound/inbound transit times for the sent/received message as
%% reason.
%%
%% The observed behaviour is that some outbound messages (that is,
%% from a connecting process to the listening process) can take an
%% unexpectedly long time to complete their journey. The more
%% connecting processes, the longer the possible delay it seems.
%%
%% eg. (With F = fun send_from_multiple_clients/2.)
%%
%%     5> F(2, 1024).
%%     {875,[{128,116},{113,139}]}
%%     6> F(4, 1024).
%%     {2995290,[{2994022,250},{2994071,80},{200,130},{211,113}]}
%%     7> F(8, 1024).
%%     {8997461,[{8996161,116},
%%               {2996471,86},
%%               {2996278,116},
%%               {2996360,95},
%%               {246,112},
%%               {213,159},
%%               {373,173},
%%               {376,118}]}
%%     8> F(8, 1024).
%%     {21001891,[{20999968,128},
%%                {8997891,172},
%%                {8997927,91},
%%                {2995716,164},
%%                {2995860,87},
%%                {134,100},
%%                {117,98},
%%                {149,125}]}
%%
%% This turns out to have been due to SCTP resends as a consequence of
%% the listener having an insufficient recbuf. Increasing the size
%% solves the problem.
%%

send_from_multiple_clients(N, Sz)
  when is_integer(N), 0 < N, is_integer(Sz), 0 < Sz ->
    timer:tc(fun listen/2, [N, <<?MAGIC, 0:Sz/unit:8>>]).

%% listen/2

listen(N, Bin) ->
    {ok, Sock} = open(),
    ok = gen_sctp:listen(Sock, true),
    {ok, PortNr} = inet:port(Sock),

    %% Spawn a middleman that in turn spawns N connecting processes,
    %% collects a list of exit reasons and then exits with the list as
    %% reason. loop/3 returns when we receive this list from the
    %% middleman's 'DOWN'.

    Self = self(),
    Fun = fun() -> exit(connect2(Self, PortNr, Bin)) end,
    {_, MRef} = spawn_monitor(fun() -> exit(fold(N, Fun)) end),
    loop(Sock, MRef, Bin).

%% fold/2
%%
%% Spawn N processes and collect their exit reasons in a list.

fold(N, Fun) ->
    start(N, Fun),
    acc(N, []).

start(0, _) ->
    ok;
start(N, Fun) ->
    spawn_monitor(Fun),
    start(N-1, Fun).

acc(0, Acc) ->
    Acc;
acc(N, Acc) ->
    receive
        {'DOWN', _MRef, process, _, RC} ->
            acc(N-1, [RC | Acc])
    end.

%% loop/3

loop(Sock, MRef, Bin) ->
    receive
        ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = Id}], B})
          when is_binary(B) ->
            Sz = size(Bin),
            {Sz, Bin} = {size(B), B},  %% assert
            ok = send(Sock, Id, mark(Bin)),
            loop(Sock, MRef, Bin);
        ?SCTP(Sock, _) ->
            loop(Sock, MRef, Bin);
        {'DOWN', MRef, process, _, Reason} ->
            Reason
    end.

%% connect2/3

connect2(Pid, PortNr, Bin) ->
    monitor(process, Pid),

    {ok, Sock} = open(),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    Id = assoc(Sock),

    %% T1 = time before send
    %% T2 = time after listening process received our message
    %% T3 = time after reply is received

    T1 = diameter_lib:now(),
    ok = send(Sock, Id, Bin),
    T2 = unmark(recv(Sock, Id)),
    T3 = diameter_lib:now(),
    {diameter_lib:micro_diff(T2, T1),  %% Outbound
     diameter_lib:micro_diff(T3, T2)}. %% Inbound

%% recv/2

recv(Sock, Id) ->
    receive
        ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = I}], Bin})
          when is_binary(Bin) ->
            Id = I,   %% assert
            Bin;
        ?SCTP(S, _) ->
            Sock = S, %% assert
            recv(Sock, Id);
        T ->
            exit(T)
    end.

%% send/3

send(Sock, Id, Bin) ->
    gen_sctp:send(Sock, Id, 0, Bin).

%% mark/1

mark(Bin) ->
    Info = term_to_binary(diameter_lib:now()),
    <<Info/binary, Bin/binary>>.

%% unmark/1

unmark(Bin) ->
    binary_to_term(Bin).

%% ===========================================================================

%% receive_what_was_sent/1
%%
%% Demonstrates reception of a message that differs from that sent.

receive_what_was_sent(_Config) ->
    send_from_multiple_clients(1, 1024*32).  %% fails

%% ===========================================================================

%% open/0

open() ->
    open([]).

%% open/1

open(Opts) ->
    gen_sctp:open([{ip, ?ADDR}, {port, 0}, {active, true}, binary,
                   {recbuf, 1 bsl 16}, {sndbuf, 1 bsl 16}
                   | Opts]).

%% assoc/1

assoc(Sock) ->
    receive
        ?SCTP(Sock, {_, #sctp_assoc_change{state = S,
                                           assoc_id = Id}}) ->
            comm_up = S,  %% assert
            Id
    end.
