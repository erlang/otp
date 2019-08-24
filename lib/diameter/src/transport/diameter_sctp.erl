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
-define(TRANSPORT_KEY, transport).

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).

%% The default port for a listener.
-define(DEFAULT_PORT, 3868).  %% RFC 3588, ch 2.1

%% How long to wait for a transport process to attach after
%% association establishment.
-define(ACCEPT_TIMEOUT, 5000).

-type connect_option() :: {raddr, inet:ip_address()}
                        | {rport, inet:port_number()}
                        | option()
                        | term(). %% gen_sctp:open_option().

-type match() :: inet:ip_address()
               | string()
               | [match()].

-type listen_option() :: {accept, match()}
                       | option()
                       | term().  %% gen_sctp:open_option().

-type option() :: {sender, boolean()}
                | sender
                | {packet, boolean() | raw}
                | {message_cb, false | diameter:eval()}.

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
         active = false :: boolean(),      %% is socket active?
         recv = true    :: boolean(),      %% should it be active?
         assoc_id :: gen_sctp:assoc_id()   %% association identifier
                   | undefined
                   | true,
         peer     :: {[inet:ip_address()], uint()} %% {RAs, RP}
                   | undefined,
         streams  :: {uint(), uint()}      %% {InStream, OutStream} counts
                   | undefined,
         os = 0   :: uint(),               %% next output stream
         rotate = 1 :: boolean() | 0 | 1,  %% rotate os?
         unordered = false :: boolean()    %% always send unordered?
                            | pos_integer(),% or if =< N outbound streams?
         packet = true :: boolean()        %% legacy transport_data?
                        | raw,
         message_cb = false :: false | diameter:eval(),
         send = false :: pid() | boolean()}).      %% sending process

%% Monitor process state.
-record(monitor,
        {transport :: pid(),
         ack = false :: boolean(),
         socket :: gen_sctp:sctp_socket(),
         assoc_id :: gen_sctp:assoc_id()}).

%% Listener process state.
-record(listener,
        {ref       :: reference(),
         socket    :: gen_sctp:sctp_socket(),
         service   :: pid(), %% service process
         pending = {0, queue:new()},
         opts      :: [[match()] | boolean() | diameter:eval()]}).
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

start(T, Svc, Opts)
  when is_list(Opts) ->
    #diameter_service{capabilities = Caps,
                      pid = Pid}
        = Svc,
    diameter_sctp_sup:start(),  %% start supervisors on demand
    Addrs = Caps#diameter_caps.host_ip_address,
    s(T, Addrs, Pid, Opts).

%% A listener spawns transports either as a consequence of this call
%% when there is not yet an association to assign it, or at comm_up on
%% a new association in which case the call retrieves a transport from
%% the pending queue.
s({accept, Ref} = A, Addrs, SvcPid, Opts) ->
    {ok, LPid, LAs} = listener(Ref, {Opts, SvcPid, Addrs}),
    try gen_server:call(LPid, {A, self()}, infinity) of
        {ok, TPid} ->
            {ok, TPid, LAs};
        No ->
            {error, No}
    catch
        exit: Reason ->
            {error, Reason}
    end;
%% This implementation is due to there being no accept call in
%% gen_sctp in order to be able to accept a new association only
%% *after* an accepting transport has been spawned.

s({connect = C, Ref}, Addrs, _SvcPid, Opts) ->
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

i(#monitor{transport = TPid} = S) ->
    monitor(process, TPid),
    putr(?TRANSPORT_KEY, TPid),
    proc_lib:init_ack({ok, self()}),
    S;

%% A process owning a listening socket.
i({listen, Ref, {Opts, SvcPid, Addrs}}) ->
    monitor(process, SvcPid),
    [_] = diameter_config:subscribe(Ref, transport), %% assert existence
    {Split, Rest} = proplists:split(Opts, [accept,
                                           packet,
                                           sender,
                                           message_cb,
                                           unordered]),
    OwnOpts = lists:append(Split),
    {LAs, Sock} = AS = open(Addrs, Rest, ?DEFAULT_PORT),
    ok = gen_sctp:listen(Sock, true),
    true = diameter_reg:add_new({?MODULE, listener, {Ref, AS}}),
    proc_lib:init_ack({ok, self(), LAs}),
    #listener{ref = Ref,
              service = SvcPid,
              socket = Sock,
              opts = [[[M] || {accept, M} <- OwnOpts],
                      proplists:get_value(packet, OwnOpts, true)
                      | [proplists:get_value(K, OwnOpts, false)
                         || K <- [sender, message_cb, unordered]]]};

%% A connecting transport.
i({connect, Pid, Opts, Addrs, Ref}) ->
    {[Ps | Split], Rest} = proplists:split(Opts, [rport,
                                                  raddr,
                                                  packet,
                                                  sender,
                                                  message_cb,
                                                  unordered]),
    OwnOpts = lists:append(Split),
    CB = proplists:get_value(message_cb, OwnOpts, false),
    false == CB orelse (Pid ! {diameter, ack}),
    RAs  = [diameter_lib:ipaddr(A) || {raddr, A} <- OwnOpts],
    [RP] = [P || {rport, P} <- Ps] ++ [P || P <- [?DEFAULT_PORT], [] == Ps],
    {LAs, Sock} = open(Addrs, Rest, 0),
    putr(?REF_KEY, Ref),
    proc_lib:init_ack({ok, self(), LAs}),
    monitor(process, Pid),
    #transport{parent = Pid,
               mode = {connect, connect(Sock, RAs, RP, [])},
               socket = Sock,
               message_cb = CB,
               unordered = proplists:get_value(ordered, OwnOpts, false),
               packet = proplists:get_value(packet, OwnOpts, true),
               send = proplists:get_value(sender, OwnOpts, false)};

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
        {K, T, Opts} when K == peeloff ->  %% association
            {sctp, Sock, _RA, _RP, _Data} = T,
            [Matches, Packet, Sender, CB, Unordered] = Opts,
            ok = accept_peer(Sock, Matches),
            demonitor(Ref, [flush]),
            false == CB orelse (S#transport.parent ! {diameter, ack}),
            t(T, S#transport{socket = Sock,
                             message_cb = CB,
                             unordered = Unordered,
                             packet = Packet,
                             send = Sender});
        accept_timeout = T ->
            x(T);
        {'DOWN', _, process, _, _} = T ->
            x(T)
    end.

%% listener/2

%% Accepting processes can be started concurrently: ensure only one
%% listener is started.
listener(Ref, T) ->
    diameter_sync:call({?MODULE, listener, Ref},
                       {?MODULE, listener, [{Ref, T}]},
                       infinity,
                       infinity).

listener({Ref, T}) ->
    l(diameter_reg:match({?MODULE, listener, {Ref, '_'}}), Ref, T).

%% Existing listening process ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _) ->
    {LAs, _Sock} = AS,
    {ok, LPid, LAs};

%% ... or not.
l([], Ref, T) ->
    diameter_sctp_sup:start_child({listen, Ref, T}).

%% open/3

open(Addrs, Opts, PortNr) ->
    case gen_sctp:open(gen_opts(portnr(addrs(Addrs, Opts), PortNr))) of
        {ok, Sock} ->
            {addrs(Sock), Sock};
        {error, Reason} ->
            x({open, Reason})
    end.

addrs(Addrs, Opts) ->
    case lists:mapfoldl(fun ipaddr/2, false, Opts) of
        {Os, true} ->
            Os;
        {_, false} ->
            Opts ++ [{ip, A} || A <- Addrs]
    end.

ipaddr({K,A}, _)
  when K == ifaddr;
       K == ip ->
    {{ip, ipaddr(A)}, true};
ipaddr(T, B) ->
    {T, B}.

ipaddr(A)
  when A == loopback;
       A == any ->
    A;
ipaddr(A) ->
    diameter_lib:ipaddr(A).

portnr(Opts, PortNr) ->
    case proplists:get_value(port, Opts) of
        undefined ->
            [{port, PortNr} | Opts];
        _ ->
            Opts
    end.

addrs(Sock) ->
    case inet:socknames(Sock) of
        {ok, As} ->
            [A || {A,_} <- As];
        {error, Reason} ->
            x({socknames, Reason})
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

handle_call({{accept, Ref}, Pid}, _, #listener{ref = Ref} = S) ->
    {TPid, NewS} = accept(Ref, Pid, S),
    {reply, {ok, TPid}, NewS};

%% Transport is telling us of parent death.
handle_call({stop, _Pid} = Reason, _From, #monitor{} = S) ->
    {stop, {shutdown, Reason}, ok, S};

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
    {noreply, #listener{} = l(T,S)};

handle_info(T, #monitor{} = S) ->
    m(T,S),
    {noreply, S}.

%% Prior to the possibility of setting pool_size on in transport
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

terminate(_, #monitor{}) ->
    ok;

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
                                              opts = Opts}
                                    = S) ->
    Id = assoc_id(Data),
    {TPid, NewS} = accept(S),
    TPid ! {peeloff, setelement(2, T, peeloff(Sock, Id, TPid)), Opts},
    setopts(Sock),
    NewS;

%% Service process has died.
l({'DOWN', _, process, Pid, _} = T, #listener{service = Pid,
                                              socket = Sock}) ->
    gen_sctp:close(Sock),
    x(T);

%% Accepting process has died.
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
down(true, TPid, #listener{pending = {N,Q}} = S) ->
    NQ = queue:filter(fun(P) -> P /= TPid end, Q),
    if N < 0 ->  %% awaiting an association ...
            S#listener{pending = {N+1, NQ}};
       true ->   %% ... or one has been assigned
            S#listener{pending = {N-1, NQ}}
    end;

%% ... or one that's already attached.
down(false, _TPid, S) ->
    S.

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
    setopts(S, recv(Data, S#transport{active = false}));

%% Outgoing message.
transition({diameter, {send, Msg}}, S) ->
    message(send, Msg, S);

%% Monitor has sent an outgoing message.
transition(Msg, S)
  when is_record(Msg, diameter_packet);
       is_binary(Msg) ->
    message(ack, Msg, S);

%% Deferred actions from a message_cb.
transition({actions, Dir, Acts}, S) ->
    setopts(ok, actions(Acts, Dir, S));

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

%% Parent process has died: call the monitor to not close the socket
%% during an ongoing send, but don't let it take forever.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid,
                                                    send = MPid}) ->
    is_boolean(MPid)
        orelse ok == (catch gen_server:call(MPid, {stop, Pid}))
        orelse exit(MPid, kill),
    stop;

%% Monitor process has died.
transition({'DOWN', _, process, MPid, _}, #transport{send = MPid})
  when is_pid(MPid) ->
    stop;

%% Timeout after transport process has been started.
transition(accept_timeout, _) ->
    ok;

%% Request for the local port number.
transition({resolve_port, Pid}, #transport{socket = Sock})
  when is_pid(Pid) ->
    Pid ! inet:port(Sock),
    ok.

%% m/2

m({Msg, StreamId}, #monitor{socket = Sock,
                            transport = TPid,
                            assoc_id = AId,
                            ack = B}) ->
    send(Sock, AId, StreamId, Msg),
    B andalso (TPid ! Msg);

m({'DOWN', _, process, TPid, _} = T, #monitor{transport = TPid}) ->
    x(T).

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

%% Start monitor process on first send.
send(Msg, #transport{send = true,
                     socket = Sock,
                     assoc_id = AId,
                     message_cb = CB}
          = S) ->
    {ok, MPid} = diameter_sctp_sup:start_child(#monitor{transport = self(),
                                                        socket = Sock,
                                                        assoc_id = AId,
                                                        ack = false /= CB}),
    monitor(process, MPid),
    send(Msg, S#transport{send = MPid});

%% Outbound Diameter message on a specified stream ...
send(#diameter_packet{transport_data = {outstream, SId}}
     = Msg,
     #transport{streams = {_, OS}}
     = S) ->
    send(SId rem OS, Msg, S);

%% ... or not: rotate when sending on multiple streams ...
send(Msg, #transport{rotate = true,
                     streams = {_, OS},
                     os = N}
          = S) ->
    send(N, Msg, S#transport{os = (N + 1) rem OS});

%% ... or send on the only stream available.
send(Msg, S) ->
    send(0, Msg, S).

%% send/3

send(StreamId, Msg, #transport{send = false,
                               socket = Sock,
                               assoc_id = AId}
                    = S) ->
    send(Sock, AId, StreamId, Msg),
    message(ack, Msg, S);

send(StreamId, Msg, #transport{send = MPid} = S) ->
    MPid ! {Msg, StreamId},
    S.

%% send/4

send(Sock, AssocId, StreamId, #diameter_packet{bin = Bin}) ->
    send(Sock, AssocId, StreamId, Bin);

send(Sock, AssocId, StreamId, Bin) ->
    case gen_sctp:send(Sock, AssocId, StreamId, Bin) of
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
    %% Deal with different association id after peeloff on Solaris by
    %% taking the id from the first reception.
    up(S#transport{assoc_id = T == accept orelse Id,
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

%% First inbound on an accepting transport.
recv({[#sctp_sndrcvinfo{assoc_id = Id}], _Bin}
     = T,
     #transport{assoc_id = true}
     = S) ->
    recv(T, S#transport{assoc_id = Id});

%% Inbound Diameter message.
recv({[#sctp_sndrcvinfo{}], Bin} = Msg, S)
  when is_binary(Bin) ->
    message(recv, Msg, recv(S));

recv({_, #sctp_shutdown_event{}}, _) ->
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

%% recv/1
%%
%% Start sending unordered after the second reception, so that an
%% outgoing CER/CEA will arrive at the peer before another request.

recv(#transport{rotate = B} = S)
  when is_boolean(B) ->
    S;

recv(#transport{rotate = 0,
                streams = {_,OS},
                socket = Sock,
                unordered = B}
     = S) ->
    ok = unordered(Sock, OS, B),
    S#transport{rotate = 1 < OS};

recv(#transport{rotate = N} = S) ->
    S#transport{rotate = N-1}.

%% unordered/3

unordered(Sock, OS, B)
  when B;
       is_integer(B), OS =< B ->
    inet:setopts(Sock, [{sctp_default_send_param,
                         #sctp_sndrcvinfo{flags = [unordered]}}]);

unordered(_, OS, B)
  when not B;
       is_integer(B), B < OS ->
    ok.

%% publish/4

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

%% setopts/2

setopts(_, #transport{socket = Sock,
                      active = A,
                      recv = B}
        = S)
  when B, not A ->
    setopts(Sock),
    S#transport{active = true};

setopts(_, #transport{} = S) ->
    S;

setopts(#transport{socket = Sock}, T) ->
    setopts(Sock),
    T.

%% setopts/1

setopts(Sock) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, Sock, X})  %% possibly on peer disconnect
    end.

%% A message_cb is invoked whenever a message is sent or received, or
%% to provide acknowledgement of a completed send or discarded
%% request. See diameter_tcp for semantics, the only difference being
%% that a recv callback can get a diameter_packet record as Msg
%% depending on how/if option packet has been specified.

%% message/3

message(send, false = M, S) ->
    message(ack, M, S);

message(ack, _, #transport{message_cb = false} = S) ->
    S;

message(Dir, Msg, S) ->
    setopts(S, actions(cb(S, Dir, Msg), Dir, S)).

%% actions/3

actions([], _, S) ->
    S;

actions([B | As], Dir, S)
  when is_boolean(B) ->
    actions(As, Dir, S#transport{recv = B});

actions([Dir | As], _, S)
  when Dir == send;
       Dir == recv ->
    actions(As, Dir, S);

actions([Msg | As], send = Dir, S)
  when is_record(Msg, diameter_packet);
       is_binary(Msg) ->
    actions(As, Dir, send(Msg, S));

actions([Msg | As], recv = Dir, #transport{parent = Pid} = S)
  when is_record(Msg, diameter_packet);
       is_binary(Msg) ->
    diameter_peer:recv(Pid, Msg),
    actions(As, Dir, S);

actions([{defer, Tmo, Acts} | As], Dir, S) ->
    erlang:send_after(Tmo, self(), {actions, Dir, Acts}),
    actions(As, Dir, S);

actions(CB, _, S) ->
    S#transport{message_cb = CB}.

%% cb/3

cb(#transport{message_cb = false, packet = P}, recv, Msg) ->
    [pkt(P, true, Msg)];

cb(#transport{message_cb = CB, packet = P}, recv = D, Msg) ->
    cb(CB, D, pkt(P, false, Msg));

cb(#transport{message_cb = CB}, Dir, Msg) ->
    cb(CB, Dir, Msg);

cb(false, send, Msg) ->
    [Msg];

cb(CB, Dir, Msg) ->
    diameter_lib:eval([CB, Dir, Msg]).

%% pkt/3

pkt(false, _, {_Info, Bin}) ->
    Bin;

pkt(true, _, {[#sctp_sndrcvinfo{stream = Id}], Bin}) ->
    #diameter_packet{bin = Bin, transport_data = {stream, Id}};

pkt(raw, true, {[Info], Bin}) ->
    #diameter_packet{bin = Bin, transport_data = Info};

pkt(raw, false, {[_], _} = Msg) ->
    Msg.
