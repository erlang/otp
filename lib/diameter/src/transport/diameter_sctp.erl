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

-module(diameter_sctp).
-behaviour(gen_server).

%% interface
-export([start/3]).

%% child start from supervisor
-export([start_link/1]).

%% child start from here
-export([init/1]).

%% gen_server callbacks
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([listener/1,%% diameter_sync callback
         info/1]).  %% service_info callback

-export([ports/0,
         ports/1]).

-export_type([listen_option/0,
              connect_option/0]).

-include_lib("kernel/include/inet_sctp.hrl").
-include_lib("diameter/include/diameter.hrl").

%% Keys into process dictionary.
-define(INFO_KEY, info).
-define(REF_KEY,  ref).

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).

%% The default port for a listener.
-define(DEFAULT_PORT, 3868).  %% RFC 3588, ch 2.1

%% Remote addresses to accept connections from.
-define(DEFAULT_ACCEPT, []).  %% any

%% How long to wait for a transport process to attach after
%% association establishment.
-define(ACCEPT_TIMEOUT, 5000).

-type connect_option() :: {raddr, inet:ip_address()}
                        | {rport, inet:port_number()}
                        | term(). %% gen_sctp:open_option().

-type match() :: inet:ip_address()
               | string()
               | [match()].

-type listen_option() :: {accept, match()}
                       | term().  %% gen_sctp:open_option().

-type uint() :: non_neg_integer().

%% Accepting/connecting transport process state.
-record(transport,
        {parent  :: pid() | undefined,
         mode :: {accept, pid()}
               | accept
               | {connect, {[inet:ip_address()], uint(), list()}}
                        %% {RAs, RP, Errors}
               | connect,
         socket   :: gen_sctp:sctp_socket() | undefined,
         assoc_id :: gen_sctp:assoc_id(),  %% association identifier
         peer     :: {[inet:ip_address()], uint()} %% {RAs, RP}
                   | undefined,
         streams  :: {uint(), uint()}      %% {InStream, OutStream} counts
                   | undefined,
         os = 0   :: uint()}).             %% next output stream

%% Listener process state.
-record(listener,
        {ref       :: reference(),
         socket    :: gen_sctp:sctp_socket(),
         count = 0 :: uint(),  %% attached transport processes
         pending = {0, queue:new()},
         accept    :: [match()]}).
%% Field pending implements two queues: the first of transport-to-be
%% processes to which an association has been assigned but for which
%% diameter hasn't yet spawned a transport process, a short-lived
%% state of affairs as a new transport is spawned as a consequence of
%% a peer being taken up, transport processes being spawned by the
%% listener on demand; the second of started transport processes that
%% have not yet been assigned an association.
%%
%% When diameter calls start/3, the transport process is either taken
%% from the first queue or spawned and placed in the second queue
%% until an association is established. When an association is
%% established, a controlling process is either taken from the second
%% queue or spawned and placed in the first queue. Thus, there are
%% only elements in one queue at a time, so share an ets table queue
%% and tag it with a positive length if it contains the first queue, a
%% negative length if it contains the second queue.

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

-spec start({accept, Ref}, #diameter_service{}, [listen_option()])
   -> {ok, pid(), [inet:ip_address()]}
 when Ref :: diameter:transport_ref();
           ({connect, Ref}, #diameter_service{}, [connect_option()])
   -> {ok, pid(), [inet:ip_address()]}
 when Ref :: diameter:transport_ref().

start(T, #diameter_service{capabilities = Caps}, Opts)
  when is_list(Opts) ->
    diameter_sctp_sup:start(),  %% start supervisors on demand
    Addrs = Caps#diameter_caps.host_ip_address,
    s(T, Addrs, lists:map(fun ip/1, Opts)).

ip({ifaddr, A}) ->
    {ip, A};
ip(T) ->
    T.

%% A listener spawns transports either as a consequence of this call
%% when there is not yet an association to assign it, or at comm_up on
%% a new association in which case the call retrieves a transport from
%% the pending queue.
s({accept, Ref} = A, Addrs, Opts) ->
    {LPid, LAs} = listener(Ref, {Opts, Addrs}),
    try gen_server:call(LPid, {A, self()}, infinity) of
        {ok, TPid} -> {ok, TPid, LAs}
    catch
        exit: Reason -> {error, Reason}
    end;
%% This implementation is due to there being no accept call in
%% gen_sctp in order to be able to accept a new association only
%% *after* an accepting transport has been spawned.

s({connect = C, Ref}, Addrs, Opts) ->
    diameter_sctp_sup:start_child({C, self(), Opts, Addrs, Ref}).

%% start_link/1

start_link(T) ->
    proc_lib:start_link(?MODULE,
                        init,
                        [T],
                        infinity,
                        diameter_lib:spawn_opts(server, [])).

%% ---------------------------------------------------------------------------
%% # info/1
%% ---------------------------------------------------------------------------

info({gen_sctp, Sock}) ->
    lists:flatmap(fun(K) -> info(K, Sock) end,
                  [{socket, socknames},
                   {peer, peernames},
                   {statistics, getstat}]).

info({K,F}, Sock) ->
    case inet:F(Sock) of
        {ok, V} ->
            [{K, map(F,V)}];
        _ ->
            []
    end.

%% inet:{sock,peer}names/1 returns [{Addr, Port}] but the port number
%% should be the same in each tuple. Map to a {[Addr], Port} tuple if
%% so.
map(K, [{_, Port} | _] = APs)
  when K == socknames;
       K == peernames ->
    try [A || {A,P} <- APs, P == Port orelse throw(?MODULE)] of
        As -> {As, Port}
    catch
        ?MODULE -> APs
    end;

map(_, V) ->
    V.

%% ---------------------------------------------------------------------------
%% # init/1
%% ---------------------------------------------------------------------------

init(T) ->
    gen_server:enter_loop(?MODULE, [], i(T)).

%% i/1

%% A process owning a listening socket.
i({listen, Ref, {Opts, Addrs}}) ->
    [_] = diameter_config:subscribe(Ref, transport), %% assert existence
    {[Matches], Rest} = proplists:split(Opts, [accept]),
    {LAs, Sock} = AS = open(Addrs, Rest, ?DEFAULT_PORT),
    ok = gen_sctp:listen(Sock, true),
    true = diameter_reg:add_new({?MODULE, listener, {Ref, AS}}),
    proc_lib:init_ack({ok, self(), LAs}),
    #listener{ref = Ref,
              socket = Sock,
              accept = [[M] || {accept, M} <- Matches]};

%% A connecting transport.
i({connect, Pid, Opts, Addrs, Ref}) ->
    {[As, Ps], Rest} = proplists:split(Opts, [raddr, rport]),
    RAs  = [diameter_lib:ipaddr(A) || {raddr, A} <- As],
    [RP] = [P || {rport, P} <- Ps] ++ [P || P <- [?DEFAULT_PORT], [] == Ps],
    {LAs, Sock} = open(Addrs, Rest, 0),
    putr(?REF_KEY, Ref),
    proc_lib:init_ack({ok, self(), LAs}),
    monitor(process, Pid),
    #transport{parent = Pid,
               mode = {connect, connect(Sock, RAs, RP, [])},
               socket = Sock};

%% An accepting transport spawned by diameter, not yet owning an
%% association.
i({accept, Ref, LPid, Pid})
  when is_pid(Pid) ->
    putr(?REF_KEY, Ref),
    proc_lib:init_ack({ok, self()}),
    monitor(process, Pid),
    MRef = monitor(process, LPid),
    wait([{peeloff, MRef}], #transport{parent = Pid,
                                       mode = {accept, LPid}});

%% An accepting transport spawned at association establishment, whose
%% parent is not yet known.
i({accept, Ref, LPid}) ->
    putr(?REF_KEY, Ref),
    proc_lib:init_ack({ok, self()}),
    erlang:send_after(?ACCEPT_TIMEOUT, self(), accept_timeout),
    MRef = monitor(process, LPid),
    wait([{parent, Ref}, {peeloff, MRef}], #transport{mode = {accept, LPid}}).

%% wait/2
%%
%% Wait for diameter to start the transport process and for the
%% association to be peeled off before processing other messages.

wait(Keys, S) ->
    lists:foldl(fun i/2, S, Keys).

i({K, Ref}, #transport{mode = {accept, _}} = S) ->
    receive
        {Ref, Pid} when K == parent ->  %% transport process started
            S#transport{parent = Pid};
        {K, T, Matches} when K == peeloff ->  %% association
            {sctp, Sock, _RA, _RP, _Data} = T,
            ok = accept_peer(Sock, Matches),
            demonitor(Ref, [flush]),
            t(T, S#transport{socket = Sock});
        accept_timeout = T ->
            x(T);
        {'DOWN', _, process, _, _} = T ->
            x(T)
    end.

%% listener/2

%% Accepting processes can be started concurrently: ensure only one
%% listener is started.
listener(LRef, T) ->
    diameter_sync:call({?MODULE, listener, LRef},
                       {?MODULE, listener, [{LRef, T}]},
                       infinity,
                       infinity).

listener({LRef, T}) ->
    l(diameter_reg:match({?MODULE, listener, {LRef, '_'}}), LRef, T).

%% Existing listening process ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _) ->
     {LAs, _Sock} = AS,
     {LPid, LAs};

%% ... or not.
l([], LRef, T) ->
    {ok, LPid, LAs} = diameter_sctp_sup:start_child({listen, LRef, T}),
    {LPid, LAs}.

%% open/3

open(Addrs, Opts, PortNr) ->
    {LAs, Os} = addrs(Addrs, Opts),
    {LAs, case gen_sctp:open(gen_opts(portnr(Os, PortNr))) of
              {ok, Sock} ->
                  Sock;
              {error, Reason} ->
                  x({open, Reason})
          end}.

addrs(Addrs, Opts) ->
    case proplists:split(Opts, [ip]) of
        {[[]], _} ->
            {Addrs, Opts ++ [{ip, A} || A <- Addrs]};
        {[As], Os} ->
            LAs = [diameter_lib:ipaddr(A) || {ip, A} <- As],
            {LAs, Os ++ [{ip, A} || A <- LAs]}
    end.

portnr(Opts, PortNr) ->
    case proplists:get_value(port, Opts) of
        undefined ->
            [{port, PortNr} | Opts];
        _ ->
            Opts
    end.

%% x/1

x(Reason) ->
    exit({shutdown, Reason}).

%% gen_opts/1

gen_opts(Opts) ->
    {L,_} = proplists:split(Opts, [binary, list, mode, active, sctp_events]),
    [[],[],[],[],[]] == L orelse ?ERROR({reserved_options, Opts}),
    [binary, {active, once} | Opts].

%% ---------------------------------------------------------------------------
%% # ports/0-1
%% ---------------------------------------------------------------------------

ports() ->
    Ts = diameter_reg:match({?MODULE, '_', '_'}),
    [{type(T), N, Pid} || {{?MODULE, T, {_, {_, S}}}, Pid} <- Ts,
                          {ok, N} <- [inet:port(S)]].

ports(Ref) ->
    Ts = diameter_reg:match({?MODULE, '_', {Ref, '_'}}),
    [{type(T), N, Pid} || {{?MODULE, T, {R, {_, S}}}, Pid} <- Ts,
                          R == Ref,
                          {ok, N} <- [inet:port(S)]].

type(listener) ->
    listen;
type(T) ->
    T.

%% ---------------------------------------------------------------------------
%% # handle_call/3
%% ---------------------------------------------------------------------------

handle_call({{accept, Ref}, Pid}, _, #listener{ref = Ref,
                                               count = K}
                                     = S) ->
    {TPid, NewS} = accept(Ref, Pid, S),
    {reply, {ok, TPid}, NewS#listener{count = K+1}};

handle_call(_, _, State) ->
    {reply, nok, State}.

%% ---------------------------------------------------------------------------
%% # handle_cast/2
%% ---------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% ---------------------------------------------------------------------------
%% # handle_info/2
%% ---------------------------------------------------------------------------

handle_info(T, #transport{} = S) ->
    {noreply, #transport{} = t(T,S)};

handle_info(T, #listener{} = S) ->
    {noreply, #listener{} = l(T,S)}.

%% Prior to the possiblity of setting pool_size on in transport
%% configuration, a new accepting transport was only started following
%% the death of a predecessor, so that there was only at most one
%% previously started transport process waiting for an association.
%% This assumption no longer holds with pool_size > 1, in which case
%% several accepting transports are started concurrently. Deal with
%% this by placing the started transports in a new queue of transport
%% processes waiting for an association.

%% ---------------------------------------------------------------------------
%% # code_change/3
%% ---------------------------------------------------------------------------

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% # terminate/2
%% ---------------------------------------------------------------------------

terminate(_, #transport{assoc_id = undefined}) ->
    ok;

terminate(_, #transport{socket = Sock}) ->
    gen_sctp:close(Sock);

terminate(_, #listener{socket = Sock}) ->
    gen_sctp:close(Sock).

%% ---------------------------------------------------------------------------

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

%% l/2
%%
%% Transition listener state.

%% Incoming message from SCTP.
l({sctp, Sock, _RA, _RP, Data} = T, #listener{socket = Sock,
                                              accept = Matches}
                                    = S) ->
    Id = assoc_id(Data),
    {TPid, NewS} = accept(S),
    TPid ! {peeloff, setelement(2, T, peeloff(Sock, Id, TPid)), Matches},
    setopts(Sock),
    NewS;

l({'DOWN', _MRef, process, TPid, _}, #listener{pending = {_,Q}} = S) ->
    down(queue:member(TPid, Q), TPid, S);

%% Transport has been removed.
l({transport, remove, _} = T, #listener{socket = Sock}) ->
    gen_sctp:close(Sock),
    x(T).

%% down/3
%%
%% Accepting transport has died.

%% One that's waiting for transport start in the pending queue ...
down(true, TPid, #listener{pending = {N,Q},
                           count = K}
                 = S) ->
    NQ = queue:filter(fun(P) -> P /= TPid end, Q),
    if N < 0 ->  %% awaiting an association ...
            S#listener{count = K-1,
                       pending = {N+1, NQ}};
       true ->   %% ... or one has been assigned
            S#listener{pending = {N-1, NQ}}
    end;

%% ... or one that's already attached.
down(false, _TPid, #listener{count = K} = S) ->
    S#listener{count = K-1}.

%% t/2
%%
%% Transition transport state.

t(T,S) ->
    case transition(T,S) of
        ok ->
            S;
        #transport{} = NS ->
            NS;
        stop ->
            x(T)
    end.

%% transition/2

%% Incoming message.
transition({sctp, Sock, _RA, _RP, Data}, #transport{socket = Sock} = S) ->
    setopts(Sock),
    recv(Data, S);

%% Outgoing message.
transition({diameter, {send, Msg}}, S) ->
    send(Msg, S);

%% Request to close the transport connection.
transition({diameter, {close, Pid}}, #transport{parent = Pid}) ->
    stop;

%% TLS over SCTP is described in RFC 3436 but has limitations as
%% described in RFC 6083. The latter describes DTLS over SCTP, which
%% addresses these limitations, DTLS itself being described in RFC
%% 4347. TLS is primarily used over TCP, which RFC 6733 acknowledges
%% by equating TLS with TLS/TCP and DTLS/SCTP.
transition({diameter, {tls, _Ref, _Type, _Bool}}, _) ->
    stop;

%% Parent process has died.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid}) ->
    stop;

%% Timeout after transport process has been started.
transition(accept_timeout, _) ->
    ok;

%% Request for the local port number.
transition({resolve_port, Pid}, #transport{socket = Sock})
  when is_pid(Pid) ->
    Pid ! inet:port(Sock),
    ok.

%% Crash on anything unexpected.

ok({ok, T}) ->
    T;
ok(T) ->
    x(T).

%% accept_peer/2

accept_peer(_, []) ->
    ok;

accept_peer(Sock, Matches) ->
    RAddrs = [A || {A,_} <- ok(inet:peernames(Sock))],
    diameter_peer:match(RAddrs, Matches)
        orelse x({accept, RAddrs, Matches}),
    ok.

%% accept/3
%%
%% Start a new transport process or use one that's already been
%% started as a consequence of diameter requesting a transport
%% process.

accept(Ref, Pid, #listener{pending = {N,_}} = S) ->
    {TPid, NQ} = q(Ref, Pid, S),
    {TPid, S#listener{pending = {N-1, NQ}}}.

%% Pending associations: attach to the first in the queue.
q(_, Pid, #listener{ref = Ref,
                    pending = {N,Q}})
  when 0 < N ->
    {TPid, _} = T = dq(Q),
    TPid ! {Ref, Pid},
    T;

%% No pending associations: spawn a new transport.
q(Ref, Pid, #listener{pending = {_,Q}}) ->
    nq({accept, Ref, self(), Pid}, Q).

%% send/2

%% Outbound Diameter message on a specified stream ...
send(#diameter_packet{bin = Bin, transport_data = {outstream, SId}},
     #transport{streams = {_, OS}}
     = S) ->
    send(SId rem OS, Bin, S),
    S;

%% ... or not: rotate through all streams.
send(#diameter_packet{bin = Bin}, S) ->
    send(Bin, S);
send(Bin, #transport{streams = {_, OS},
                     os = N}
          = S)
  when is_binary(Bin) ->
    send(N, Bin, S),
    S#transport{os = (N + 1) rem OS}.

%% send/3

send(StreamId, Bin, #transport{socket = Sock,
                               assoc_id = AId}) ->
    send(Sock, AId, StreamId, Bin).

%% send/4

send(Sock, AssocId, Stream, Bin) ->
    case gen_sctp:send(Sock, AssocId, Stream, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            x({send, Reason})
    end.

%% recv/2

%% Association established ...
recv({_, #sctp_assoc_change{state = comm_up,
                            outbound_streams = OS,
                            inbound_streams = IS,
                            assoc_id = Id}},
     #transport{assoc_id = undefined,
                mode = {T, _},
                socket = Sock}
     = S) ->
    Ref = getr(?REF_KEY),
    publish(T, Ref, Id, Sock),
    up(S#transport{assoc_id = Id,
                   streams = {IS, OS}});

%% ... or not: try the next address.
recv({_, #sctp_assoc_change{} = E},
     #transport{assoc_id = undefined,
                socket = Sock,
                mode = {connect = C, {[RA|RAs], RP, Es}}}
     = S) ->
    S#transport{mode = {C, connect(Sock, RAs, RP, [{RA,E} | Es])}};

%% Association failure.
recv({_, #sctp_assoc_change{}}, _) ->
    stop;

%% Inbound Diameter message.
recv({[#sctp_sndrcvinfo{stream = Id}], Bin}, #transport{parent = Pid})
  when is_binary(Bin) ->
    diameter_peer:recv(Pid, #diameter_packet{transport_data = {stream, Id},
                                             bin = Bin}),
    ok;

recv({_, #sctp_shutdown_event{assoc_id = A}},
     #transport{assoc_id = Id})
  when A == Id;
       A == 0 ->
    stop;

%% Note that diameter_sctp(3) documents that sctp_events cannot be
%% specified in the list of options passed to gen_sctp and that
%% gen_opts/1 guards against this. This is to ensure that we know what
%% events to expect and also to ensure that we receive
%% #sctp_sndrcvinfo{} with each incoming message (data_io_event =
%% true). Adaptation layer events (ie. #sctp_adaptation_event{}) are
%% disabled by default so don't handle it. We could simply disable
%% events we don't react to but don't.

recv({_, #sctp_paddr_change{}}, _) ->
    ok;

recv({_, #sctp_pdapi_event{}}, _) ->
    ok.

publish(T, Ref, Id, Sock) ->
    true = diameter_reg:add_new({?MODULE, T, {Ref, {Id, Sock}}}),
    putr(?INFO_KEY, {gen_sctp, Sock}).  %% for info/1

%% up/1

up(#transport{parent = Pid,
              mode = {connect = C, {[RA | _], RP, _}}}
   = S) ->
    diameter_peer:up(Pid, {RA,RP}),
    S#transport{mode = C};

up(#transport{parent = Pid,
              mode = {accept = A, _}}
   = S) ->
    diameter_peer:up(Pid),
    S#transport{mode = A}.

%% accept/1
%%
%% Start a new transport process or use one that's already been
%% started as a consequence of an event to a listener process.

accept(#listener{pending = {N,_}} = S) ->
    {TPid, NQ} = q(S),
    {TPid, S#listener{pending = {N+1, NQ}}}.

%% Transport waiting for an association: use it.
q(#listener{pending = {N,Q}})
  when N < 0 ->
    dq(Q);

%% No transport start yet: spawn one and queue.
q(#listener{ref = Ref,
            pending = {_,Q}}) ->
    nq({accept, Ref, self()}, Q).

%% nq/2
%%
%% Place a transport process in the second pending queue to make it
%% available to the next association.

nq(Arg, Q) ->
    {ok, TPid} = diameter_sctp_sup:start_child(Arg),
    monitor(process, TPid),
    {TPid, queue:in(TPid, Q)}.

%% dq/1
%%
%% Remove a transport process from the first pending queue to assign
%% it to an existing association.

dq(Q) ->
    {{value, TPid}, NQ} = queue:out(Q),
    {TPid, NQ}.

%% assoc_id/1
%%
%% It's unclear if this is needed, or if the first message on an
%% association is always sctp_assoc_change, but don't assume since
%% SCTP behaviour differs between operating systems.

assoc_id({[#sctp_sndrcvinfo{assoc_id = Id}], _}) ->
    Id;
assoc_id({_, Rec}) ->
    id(Rec).

id(#sctp_shutdown_event{assoc_id = Id}) ->
    Id;
id(#sctp_assoc_change{assoc_id = Id}) ->
    Id;
id(#sctp_sndrcvinfo{assoc_id = Id}) ->
    Id;
id(#sctp_paddr_change{assoc_id = Id}) ->
    Id;
id(#sctp_adaptation_event{assoc_id = Id}) ->
    Id.

%% peeloff/3

peeloff(LSock, Id, TPid) ->
    {ok, Sock} = gen_sctp:peeloff(LSock, Id),
    ok = gen_sctp:controlling_process(Sock, TPid),
    Sock.

%% connect/4

connect(_, [], _, Reasons) ->
    x({connect, lists:reverse(Reasons)});

connect(Sock, [Addr | AT] = As, Port, Reasons) ->
    case gen_sctp:connect_init(Sock, Addr, Port, []) of
        ok ->
            {As, Port, Reasons};
        {error, _} = E ->
            connect(Sock, AT, Port, [{Addr, E} | Reasons])
    end.

%% setopts/1

setopts(Sock) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, Sock, X})  %% possibly on peer disconnect
    end.
