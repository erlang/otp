%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
-export([send_one_from_many/1, send_one_from_many/0,
         send_many_from_one/1, send_many_from_one/0,
         receive_what_was_sent/1]).

-include_lib("kernel/include/inet_sctp.hrl").

%% Message from gen_sctp are of this form.
-define(SCTP(Sock, Data), {sctp, Sock, _, _, Data}).

%% Open sockets on the loopback address.
-define(ADDR, {127,0,0,1}).

%% An indescribably long number of milliseconds after which everthing
%% that should have happened has.
-define(FOREVER, 2000).

%% How many milliseconds to tolerate between the fastest and slowest
%% turnaround times.
-define(VARIANCE, 100).

%% The first byte in each message we send as a simple guard against
%% not receiving what was sent.
-define(MAGIC, 0).

%% Requested number of inbound/outbound streams.
-define(STREAMS, 5).

%% Success for send_multiple. Match in each testcase rather than in
%% send_multiple itself for a better failure in common_test.
-define(OK, {_, true, _, [true, true], [], _}).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [send_one_from_many,
     send_many_from_one,
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

%% send_one_from_many/0
%%
%% Demonstrates sluggish delivery of messages.

send_one_from_many() ->
    [{timetrap, {seconds, 30}}].

send_one_from_many(_) ->
    ?OK = send_multiple(128, 1, 1024).

%% send_one_from_many/2
%%
%% Opens a listening socket and then spawns a specified number of
%% processes, each of which connects, sends a message, receives a
%% reply, and exits.
%%
%% Returns the elapsed time for all connecting process to exit
%% together with a list of exit reasons. In the successful case a
%% connecting process exits with the outbound/inbound transit times
%% for the sent/received message as reason.
%%
%% The observed behaviour is that some outbound messages (that is,
%% from a connecting process to the listening process) can take an
%% unexpectedly long time to complete their journey. The more
%% connecting processes, the longer it can take it seems.
%%
%% eg. 5> send_one_from_many(2, 1024).
%%     {875,[{128,116},{113,139}]}
%%     6> send_one_from_many(4, 1024).
%%     {2995290,[{2994022,250},{2994071,80},{200,130},{211,113}]}
%%     7> send_one_from_many(8, 1024).
%%     {8997461,[{8996161,116},
%%               {2996471,86},
%%               {2996278,116},
%%               {2996360,95},
%%               {246,112},
%%               {213,159},
%%               {373,173},
%%               {376,118}]}
%%     8> send_one_from_many(8, 1024).
%%     {21001891,[{20999968,128},
%%                {8997891,172},
%%                {8997927,91},
%%                {2995716,164},
%%                {2995860,87},
%%                {134,100},
%%                {117,98},
%%                {149,125}]}
%%

send_multiple(Clients, Msgs, Sz)
  when is_integer(Clients), 0 < Clients,
       is_integer(Msgs), 0 < Msgs,
       is_integer(Sz), 0 < Sz ->
    T0 = diameter_lib:now(),
    {S, Res} = timer:tc(fun listen/3, [Clients, Msgs, Sz]),
    report(T0, Res),
    Ts = lists:append(Res),
    Outgoing = [DT || {_,{_,_,DT},{_,_,_},_} <- Ts],
    Incoming = [DT || {_,{_,_,_},{_,_,DT},_} <- Ts],
    Diffs = [lists:max(L) - lists:min(L) || L <- [Outgoing, Incoming]],
    {S,
     S < ?FOREVER*1000,
     Diffs,
     [D < V || V <- [?VARIANCE*1000], D <- Diffs],
     [T || T <- Ts, [] == [T || {_,{_,_,_},{_,_,_},_} <- [T]]],
     Res}.

%% listen/3

listen(Clients, Msgs, Sz) ->
    {ok, Sock} = open(),
    ok = gen_sctp:listen(Sock, true),
    {ok, PortNr} = inet:port(Sock),

    %% Spawn a middleman that in turn spawns N connecting processes,
    %% collects a list of exit reasons and then exits with the list as
    %% reason. accept/2 returns when we receive this list from the
    %% middleman's 'DOWN'.

    Self = self(),
    Fun = fun() -> exit(client(Self, PortNr, Msgs, Sz)) end,  %% start clients
    {_, MRef} = spawn_monitor(fun() -> exit(clients(Clients, Fun)) end),
    accept_loop(Sock, MRef).

%% fclients/2
%%
%% Spawn N processes and collect their exit reasons in a list.

clients(N, Fun) ->
    start(N, Fun),
    acc(N, []).

%% start/2

start(0, _) ->
    ok;

start(N, Fun) ->
    spawn_monitor(Fun),
    start(N-1, Fun).

%% acc/2

acc(0, Acc) ->
    Acc;

acc(N, Acc) ->
    receive
        {'DOWN', _MRef, process, _, RC} ->
            acc(N-1, [RC | Acc])
    end.

%% accept_loop/2

accept_loop(Sock, MRef) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
        ?SCTP(Sock, {_, #sctp_assoc_change{state = comm_up,
                                           outbound_streams = OS,
                                           assoc_id = Id}}) ->
            Self = self(),
            TPid = spawn(fun() -> assoc(monitor(process, Self), Id, OS) end),
            NewSock = peeloff(Sock, Id, TPid),
            TPid ! {peeloff, NewSock},
            accept_loop(Sock, MRef);
        ?SCTP(Sock, _) ->
            accept_loop(Sock, MRef);
        {'DOWN', MRef, process, _, Reason} ->
            Reason;
        T ->
            error(T)
    end.

%% assoc/3
%%
%% Server process that answers incoming messages as long as the parent
%% lives.

assoc(MRef, _Id, OS)
  when is_reference(MRef) ->
    {peeloff, Sock} = receive T -> T end,
    recv_loop(Sock, false, sender(Sock, false, OS), MRef).

%% recv_loop/4

recv_loop(Sock, Id, Pid, MRef) ->
    ok = inet:setopts(Sock, [{active, once}]),
    recv(Sock, Id, Pid, MRef, receive T -> T end).

%% recv/5

%% Association id can change on a peeloff socket on some versions of
%% Solaris.
recv(Sock,
     false,
     Pid,
     MRef,
     ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = Id}], _})
     = T) ->
    Pid ! {assoc_id, Id},
    recv(Sock, Id, Pid, MRef, T);

recv(Sock, Id, Pid, MRef, ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = I}], B}))
  when is_binary(B) ->
    T2 = diameter_lib:now(),
    Id = I,                      %% assert
    <<?MAGIC, Bin/binary>> = B,  %% assert
    {[_,_,_,Sz] = L, Bytes} = unmark(Bin),
    Sz = size(Bin) - Bytes,      %% assert
    <<_:Bytes/binary, Body:Sz/binary>> = Bin,
    send(Pid, [T2|L], Body),  %% answer
    recv_loop(Sock, Id, Pid, MRef);

recv(Sock, Id, Pid, MRef, ?SCTP(Sock, _)) ->
    recv_loop(Sock, Id, Pid, MRef);

recv(_, _, _, MRef, {'DOWN', MRef, process, _, Reason}) ->
    Reason;

recv(_, _, _, _, T) ->
    error(T).

%% send/3

send(Pid, Header, Body) ->
    Pid ! {send, Header, Body}.

%% sender/3
%%
%% Start a process that sends, so as not to block the controlling process.

sender(Sock, Id, OS) ->
    Pid = self(),
    spawn(fun() -> send_loop(Sock, Id, OS, 1, monitor(process, Pid)) end).

%% send_loop/5

send_loop(Sock, Id, OS, N, MRef) ->
    receive
        {assoc_id, I} ->
            send_loop(Sock, I, OS, N, MRef);
        {send, L, Body} ->
            Stream = N rem OS,
            ok = send(Sock, Id, Stream, mark(Body, [N, Stream | L])),
            send_loop(Sock, Id, OS, N+1, MRef);
        {'DOWN', MRef, process, _, _} = T ->
            T;
        T ->
            error(T)
    end.

%% peeloff/3

peeloff(LSock, Id, TPid) ->
    {ok, Sock} = gen_sctp:peeloff(LSock, Id),
    ok = gen_sctp:controlling_process(Sock, TPid),
    Sock.

%% client/4

client(Pid, PortNr, Msgs, Sz) ->
    monitor(process, Pid),
    {ok, Sock} = open(),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    recv_loop(Sock, Msgs, Sz).

%% recv_loop/3

recv_loop(_, 0, T) ->
    [_,_|Acc] = T,
    Acc;

recv_loop(Sock, Msgs, T) ->
    ok = inet:setopts(Sock, [{active, once}]),
    {I, NewT} = recv(Sock, Msgs, T, receive X -> X end),
    recv_loop(Sock, Msgs - I, NewT).

%% recv/4

recv(Sock, Msgs, Sz, ?SCTP(Sock, {_, #sctp_assoc_change{} = A})) ->
    #sctp_assoc_change{state = comm_up,  %% assert
                       assoc_id = Id,
                       outbound_streams = OS}
        = A,
    true = is_integer(Sz),  %% assert
    send_n(Msgs, sender(Sock, Id, OS), Sz),
    {0, [Id, OS]};

recv(Sock, _, T, ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = Id}], Bin})) ->
    T4 = diameter_lib:now(),
    [Id, OS | Acc] = T,
    {1, [Id, OS, stat(T4, Bin) | Acc]};

recv(Sock, _, T, ?SCTP(Sock, _)) ->
    {0, [_,_|_] = T};

recv(_, _, _, T) ->
    error(T).

%% send_n/3
%%
%% Send messages to the server from dedicated processes.

send_n(0, _, _) ->
    ok;

send_n(N, Pid, Sz) ->
    M = rand:uniform(255),
    send(Pid, [Sz], binary:copy(<<M>>, Sz)),
    send_n(N-1, Pid, Sz).

%% send/4

send(Sock, Id, Stream, Bin) ->
    case gen_sctp:send(Sock, Id, Stream, <<?MAGIC, Bin/binary>>) of
        {error, eagain} ->
            send(Sock, Id, Stream, Bin);
        RC ->
            RC
    end.

%% stat/2

stat(T4, <<?MAGIC, Bin/binary>>) ->
    %% T1 = time at send
    %% T2 = time at reception by server
    %% T3 = time at reception by server's sender
    %% T4 = time at reception of answer

    {[T3,NI,SI,T2,T1,NO,SO,Sz], Bytes} = unmark(Bin),

    Sz = size(Bin) - Bytes,  %% assert

    {T1,
     {NO, SO, diameter_lib:micro_diff(T2, T1)},  %% Outbound
     {NI, SI, diameter_lib:micro_diff(T4, T3)},  %% Inbound
     T4}.

%% mark/2

mark(Bin, T) ->
    Info = term_to_binary([diameter_lib:now() | T]),
    <<Info/binary, Bin/binary>>.

%% unmark/1

unmark(Bin) ->
    T = binary_to_term(Bin),
    {T, size(term_to_binary(T))}.

%% ===========================================================================

%% send_many_from_one/0
%%
%% Demonstrates sluggish delivery of messages.

send_many_from_one() ->
    [{timetrap, {seconds, 30}}].

send_many_from_one(_) ->
    ?OK = send_multiple(1, 128, 1024).

%% ===========================================================================

%% receive_what_was_sent/1
%%
%% Demonstrates reception of a message that differs from that sent.

receive_what_was_sent(_Config) ->
    ?OK = send_multiple(1, 1, 1024*32).

%% ===========================================================================

%% open/0

open() ->
    open([]).

%% open/1

open(Opts) ->
    gen_sctp:open([{ip, ?ADDR}, {port, 0}, {active, false}, binary,
                   {sctp_initmsg, #sctp_initmsg{num_ostreams = ?STREAMS,
                                                max_instreams = ?STREAMS}},
                   {recbuf, 1 bsl 16}, {sndbuf, 1 bsl 16}
                   | Opts]).

%% report/2

report(T0, Ts) ->
    ct:pal("~p~n", [lists:sort([sort([{diameter_lib:micro_diff(T1,T0),
                                       OT,
                                       IT,
                                       diameter_lib:micro_diff(T4,T0)}
                                      || {T1,OT,IT,T4} <- L])
                                || L <- Ts])]).

%% sort/1

sort(L) ->
    lists:sort(fun({_,{N,_,_},_,_}, {_,{M,_,_},_,_}) -> N =< M end, L).
