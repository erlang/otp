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
%% This module implements (as a process) the RFC 3588/6733 Peer State
%% Machine modulo the necessity of adapting the peer election to the
%% fact that we don't know the identity of a peer until we've received
%% a CER/CEA from it.
%%

-module(diameter_peer_fsm).
-behaviour(gen_server).

%% Interface towards diameter_watchdog.
-export([start/3,
         result_code/2]).

%% Interface towards diameter.
-export([find/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% diameter_peer_fsm_sup callback
-export([start_link/1]).

%% internal callbacks
-export([match/1]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% Values of Disconnect-Cause in DPR.
-define(GOAWAY, 2).  %% DO_NOT_WANT_TO_TALK_TO_YOU
-define(BUSY,   1).  %% BUSY
-define(REBOOT, 0).  %% REBOOTING

%% Values of Inband-Security-Id.
-define(NO_INBAND_SECURITY, 0).
-define(TLS, 1).

%% Note that the a common dictionary hrl is purposely not included
%% since the common dictionary is an argument to start/3.

%% Keys in process dictionary.
-define(CB_KEY, cb).         %% capabilities callback
-define(DPR_KEY, dpr).       %% disconnect callback
-define(DPA_KEY, dpa).       %% timeout for incoming DPA, or shutdown after
                             %% outgoing DPA
-define(REF_KEY, ref).       %% transport_ref()
-define(Q_KEY, q).           %% transport start queue
-define(START_KEY, start).   %% start of connected transport
-define(SEQUENCE_KEY, mask). %% mask for sequence numbers
-define(RESTRICT_KEY, restrict). %% nodes for connection check

%% The default sequence mask.
-define(NOMASK, {0,32}).

%% A 2xxx series Result-Code. Not necessarily 2001.
-define(IS_SUCCESS(N), 2 == (N) div 1000).

%% Guards.
-define(IS_UINT32(N), (is_integer(N) andalso 0 =< N andalso 0 == N bsr 32)).
-define(IS_TIMEOUT(N), ?IS_UINT32(N)).
-define(IS_CAUSE(N), N == ?REBOOT; N == rebooting;
                     N == ?GOAWAY; N == goaway;
                     N == ?BUSY;   N == busy).

%% RFC 6733:
%%
%%   Timeout        An application-defined timer has expired while waiting
%%                  for some event.
%%

%% Default timeout for reception of CER/CEA.
-define(CAPX_TIMEOUT, 10000).

%% Default timeout for DPA to be received in response to an outgoing
%% DPR. A bit short but the timeout used to be hardcoded. (So it could
%% be worse.)
-define(DPA_TIMEOUT, 1000).

%% Default timeout for the connection to be closed by the peer
%% following an outgoing DPA in response to an incoming DPR. It's the
%% recipient of DPA that should close the connection according to the
%% RFC.
-define(DPR_TIMEOUT, 5000).

-type uint32() :: diameter:'Unsigned32'().

-record(state,
        {state %% of RFC 3588 Peer State Machine
              :: {'Wait-Conn-Ack', uint32()}
               | recv_CER
               | {'Wait-CEA', uint32(), uint32()}
               | 'Open',
         mode :: accept | connect | {connect, reference()},
         parent       :: pid(),     %% watchdog process
         transport    :: pid(),     %% transport process
         dictionary   :: module(),  %% common dictionary
         service      :: #diameter_service{} | undefined,
         dpr = false  :: false
                       | true  %% DPR received, DPA sent
                       | {boolean(), uint32(), uint32()},
                       %% hop by hop and end to end identifiers in
                       %% outgoing DPR; boolean says whether or not
                       %% the request was sent explicitly with
                       %% diameter:call/4.
         codec :: #{decode_format := diameter:decode_format(),
                    string_decode := boolean(),
                    strict_mbit := boolean(),
                    rfc := 3588 | 6733,
                    ordered_encode := false},
         strict :: boolean(),
         ack = false :: boolean(),
         length_errors :: exit | handle | discard,
         incoming_maxlen :: integer() | infinity}).

%% There are non-3588 states possible as a consequence of 5.6.1 of the
%% standard and the corresponding problem for incoming CEA's: we don't
%% know who we're talking to until either a CER or CEA has been
%% received. The CEA problem in particular makes it impossible to
%% follow the state machine exactly as documented in 3588: there can
%% be no election until the CEA arrives and we have an Origin-Host to
%% elect.

%%
%% Once upon a time start/2 started a process akin to that started by
%% start/3 below, which in turn started a watchdog/transport process
%% with the result that the watchdog could send DWR/DWA regardless of
%% whether or not the corresponding Peer State Machine was in its open
%% state; that is, before capabilities exchange had taken place. This
%% is not what RFC's 3588 and 3539 say (albeit not very clearly).
%% Watchdog messages are only exchanged on *open* connections, so the
%% 3539 state machine is more naturally placed on top of the 3588 Peer
%% State Machine rather than closer to the transport. This is what we
%% now do below: connect/accept call diameter_watchdog and return the
%% pid of the watchdog process, and the watchdog in turn calls start/3
%% below to start the process implementing the Peer State Machine.
%%

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

-spec start(T, [Opt], {map(), [node()], module(), #diameter_service{}})
   -> {reference(), pid()}
 when T   :: {connect|accept, diameter:transport_ref()},
      Opt :: diameter:transport_opt().

%% diameter_config requires a non-empty list of applications on the
%% service but diameter_service then constrains the list to any
%% specified on the transport in question. Check here that the list is
%% still non-empty.

start({_,_} = Type, Opts, S) ->
    Ack = make_ref(),
    T = {Ack, self(), Type, Opts, S},
    {ok, Pid} = diameter_peer_fsm_sup:start_child(T),
    try
        {erlang:monitor(process, Pid), Pid}
    after
        Pid ! Ack
    end.

start_link(T) ->
    {ok, _} = proc_lib:start_link(?MODULE,
                                  init,
                                  [T],
                                  infinity,
                                  diameter_lib:spawn_opts(server, [])).

%% find/1
%%
%% Identify both pids of a peer_fsm/transport pair.

find(Pid) ->
    findl([{?MODULE, '_', Pid}, {?MODULE, Pid, '_'}]).

findl([]) ->
    false;

findl([Pat | Rest]) ->
    try
        [{{_, Pid, TPid}, Pid}] = diameter_reg:match(Pat),
        {Pid, TPid}
    catch
        error:_ ->
            findl(Rest)
    end.

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

%% init/1

init(T) ->
    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], i(T)).

i({Ack, WPid, {M, Ref} = T, Opts, {SvcOpts, Nodes, Dict0, Svc}}) ->
    erlang:monitor(process, WPid),
    wait(Ack, WPid),
    diameter_stats:reg(Ref),

    #{sequence := Mask, incoming_maxlen := Maxlen}
        = SvcOpts,

    {[Cs,Ds], Rest} = proplists:split(Opts, [capabilities_cb, disconnect_cb]),
    putr(?CB_KEY, {Ref, [F || {_,F} <- Cs]}),
    putr(?DPR_KEY, [F || {_, F} <- Ds]),
    putr(?REF_KEY, Ref),
    putr(?SEQUENCE_KEY, Mask),
    putr(?RESTRICT_KEY, Nodes),
    putr(?DPA_KEY, {proplists:get_value(dpr_timeout, Opts, ?DPR_TIMEOUT),
                    proplists:get_value(dpa_timeout, Opts, ?DPA_TIMEOUT)}),

    Tmo = proplists:get_value(capx_timeout, Opts, ?CAPX_TIMEOUT),
    Strict = proplists:get_value(strict_capx, Opts, true),
    LengthErr = proplists:get_value(length_errors, Opts, exit),

    {TPid, Addrs} = start_transport(T, Rest, Svc),

    diameter_reg:add({?MODULE, self(), TPid}),  %% lets pairs be discovered

    #state{state = {'Wait-Conn-Ack', Tmo},
           parent = WPid,
           transport = TPid,
           dictionary = Dict0,
           mode = M,
           service = svc(Svc, Addrs),
           length_errors = LengthErr,
           strict = Strict,
           incoming_maxlen = Maxlen,
           codec = maps:with([decode_format,
                              string_decode,
                              strict_mbit,
                              rfc,
                              ordered_encode],
                             SvcOpts#{ordered_encode => false})}.
%% The transport returns its local ip addresses so that different
%% transports on the same service can use different local addresses.
%% The local addresses are put into Host-IP-Address avps here when
%% sending capabilities exchange messages.

%% Wait for the caller to have a monitor to avoid a race with our
%% death. (Since the exit reason is used in diameter_service.)
wait(Ref, Pid) ->
    receive
        Ref ->
            ok;
        {'DOWN', _, process, Pid, _} = D ->
            x(D)
    end.

x(T) ->
    exit({shutdown, T}).

start_transport(T, Opts, #diameter_service{capabilities = LCaps} = Svc) ->
    Addrs0 = LCaps#diameter_caps.host_ip_address,
    start_transport(Addrs0, {T, Opts, Svc}).

start_transport(Addrs0, T) ->
    case diameter_peer:start(T) of
        {TPid, Addrs, Tmo, Data} ->
            erlang:monitor(process, TPid),
            q_next(TPid, Addrs0, Tmo, Data),
            {TPid, Addrs};
        {error, No} ->
            x({no_connection, No})
    end.

svc(#diameter_service{capabilities = LCaps0} = Svc, Addrs) ->
    #diameter_caps{host_ip_address = Addrs0}
        = LCaps0,
    case Addrs0 of
        [] ->
            LCaps = LCaps0#diameter_caps{host_ip_address = Addrs},
            Svc#diameter_service{capabilities = LCaps};
        [_|_] ->
            Svc
    end.

readdr(#diameter_service{capabilities = LCaps0} = Svc, Addrs) ->
    LCaps = LCaps0#diameter_caps{host_ip_address = Addrs},
    Svc#diameter_service{capabilities = LCaps}.

%% The 4-tuple Data returned from diameter_peer:start/1 identifies the
%% transport module/config use to start the transport process in
%% question as well as any alternates to try if a connection isn't
%% established within Tmo.
q_next(TPid, Addrs0, Tmo, {_,_,_,_} = Data) ->
    send_after(Tmo, {connection_timeout, TPid}),
    putr(?Q_KEY, {Addrs0, Tmo, Data}).

%% Connection has been established: retain the started
%% pid/module/config in the process dictionary. This is a part of the
%% interface defined by this module, so that the transport pid can be
%% found when constructing service_info (in order to extract further
%% information from it).
keep_transport(TPid) ->
    {_, _, {{_,_,_} = T, _, _, _}} = eraser(?Q_KEY),
    putr(?START_KEY, {TPid, T}).

send_after(infinity, _) ->
    ok;
send_after(Tmo, T) ->
    erlang:send_after(Tmo, self(), T).

%% handle_call/3

handle_call(_, _, State) ->
    {reply, nok, State}.

%% handle_cast/2

handle_cast(_, State) ->
    {noreply, State}.

%% handle_info/1

handle_info(T, #state{} = State) ->
    try transition(T, State) of
        ok ->
            {noreply, State};
        #state{state = X} = S ->
            ?LOGC(X /= State#state.state, transition, X),
            {noreply, S};
        {stop, Reason} ->
            ?LOG(stop, Reason),
            {stop, {shutdown, Reason}, State};
        stop ->
            ?LOG(stop, truncate(T)),
            {stop, {shutdown, T}, State}
    catch
        exit: {diameter_codec, encode, T} = Reason ->
            incr_error(send, T, State#state.dictionary),
            ?LOG(stop, Reason),
            {stop, {shutdown, Reason}, State};
        {?MODULE, Tag, Reason}  ->
            ?LOG(stop, Tag),
            {stop, {shutdown, Reason}, State}
    end.
%% The form of the throw caught here is historical. It's
%% significant that it's not a 2-tuple, as in ?FAILURE(Reason),
%% since these are caught elsewhere.

%% Note that there's no guarantee that the service and transport
%% capabilities are good enough to build a CER/CEA that can be
%% successfully encoded. It's not checked at diameter:add_transport/2
%% since this can be called before creating the service.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

truncate({'DOWN' = T, _, process, Pid, _}) ->
    {T, Pid};
truncate(T) ->
    T.

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

eraser(Key) ->
    erase({?MODULE, Key}).

%% transition/2

%% Connection to peer.
transition({diameter, {TPid, connected, Remote}},
           #state{transport = TPid,
                  state = PS,
                  mode = M}
           = S) ->
    {'Wait-Conn-Ack', _} = PS,  %% assert
    connect = M,                %%
    keep_transport(TPid),
    send_CER(S#state{mode = {M, Remote}});

transition({diameter, {TPid, connected, Remote, LAddrs}},
           #state{transport = TPid,
                  service = Svc}
           = S) ->
    transition({diameter, {TPid, connected, Remote}},
               S#state{service = svc(Svc, LAddrs)});

%% Connection from peer.
transition({diameter, {TPid, connected}},
           #state{transport = TPid,
                  state = PS,
                  mode = M,
                  parent = Pid}
           = S) ->
    {'Wait-Conn-Ack', Tmo} = PS,  %% assert
    accept = M,                   %%
    keep_transport(TPid),
    Pid ! {accepted, self()},
    start_timer(Tmo, S#state{state = recv_CER});

%% Connection established after receiving a connection_timeout
%% message. This may be followed by an incoming message which arrived
%% before the transport was killed and this can't be distinguished
%% from one from the transport that's been started to replace it.
transition({diameter, T}, _)
  when tuple_size(T) < 5, connected == element(2,T) ->
    {stop, connection_timeout};

%% Connection has timed out: start an alternate.
transition({connection_timeout = T, TPid},
           #state{transport = TPid,
                  state = {'Wait-Conn-Ack', _}}
           = S) ->
    exit(TPid, {shutdown, T}),
    start_next(S);

%% Connect timeout after connection or alternate start: ignore.
transition({connection_timeout, _}, _) ->
    ok;

%% Requests for acknowledgements to the transport.
transition({diameter, ack}, S) ->
    S#state{ack = true};

%% Incoming message from the transport.
transition({diameter, {recv, Msg}}, S) ->
    incoming(recv(Msg, S), S);

%% Handler of an incoming request is telling of its existence.
transition({handler, Pid}, _) ->
    put_route(Pid),
    ok;

%% Timeout when still in the same state ...
transition({timeout = T, PS}, #state{state = PS}) ->
    {stop, {capx(PS), T}};

%% ... or not.
transition({timeout, _}, _) ->
    ok;

%% Outgoing message.
transition({send, Msg}, S) ->
    outgoing(Msg, S);
transition({send, Msg, Route}, S) ->
    route_outgoing(Route),
    outgoing(Msg, S);

%% Request for graceful shutdown at remove_transport, stop_service of
%% application shutdown.
transition({shutdown, Pid, Reason}, #state{parent = Pid, dpr = false} = S) ->
    dpr(Reason, S);
transition({shutdown, Pid, _}, #state{parent = Pid}) ->
    ok;

%% DPA reception has timed out, or peer has not closed the connection
%% as a result of outgoing DPA.
transition(dpa_timeout, _) ->
    stop;

%% Someone wants to know a resolved port: forward to the transport process.
transition({resolve_port, _Pid} = T, #state{transport = TPid}) ->
    TPid ! T,
    ok;

%% Parent has died.
transition({'DOWN', _, process, WPid, _},
           #state{parent = WPid}) ->
    stop;

%% Transport has died before connection timeout.
transition({'DOWN', _, process, TPid, _},
           #state{transport = TPid}
           = S) ->
    start_next(S#state{ack = false});

%% Transport has died after connection timeout, or handler process has
%% died.
transition({'DOWN', _, process, Pid, _}, #state{transport = TPid}) ->
    is_reference(erase_route(Pid))
        andalso send(TPid, false),  %% answer not forthcoming
    ok;

%% State query.
transition({state, Pid}, #state{state = S, transport = TPid}) ->
    Pid ! {self(), [S, TPid]},
    ok.

%% Crash on anything unexpected.

%% route_outgoing/1

%% Map identifiers in an outgoing request to be able to lookup the
%% handler process when the answer is received.
route_outgoing({Pid, Ref, Seqs}) ->  %% request
    MRef = monitor(process, Pid),
    put(Pid, Seqs),
    put(Seqs, {Pid, Ref, MRef});

%% Remove a mapping made for an incoming request.
route_outgoing(Pid)
  when is_pid(Pid) ->  %% answer
    MRef = erase_route(Pid),
    undefined == MRef orelse demonitor(MRef).

%% put_route/1

%% Monitor on a handler process for an incoming request.
put_route(Pid) ->
    MRef = monitor(process, Pid),
    put(Pid, MRef).

%% get_route/3

%% Incoming answer.
get_route(_, _, #diameter_packet{header = #diameter_header{is_request = false}}
                = Pkt) ->
    Seqs = diameter_codec:sequence_numbers(Pkt),
    case erase(Seqs) of
        {Pid, Ref, MRef} ->
            demonitor(MRef),
            erase(Pid),
            {Pid, Ref, self()};
        undefined ->  %% request unknown
            false
    end;

%% Requests answered here ...
get_route(_, N, _)
  when N == 'CER';
       N == 'DPR' ->
    false;

%% ... or not.
get_route(Ack, _, _) ->
    Ack.

%% erase_route/1

erase_route(Pid) ->
    case erase(Pid) of
        {_,_} = Seqs ->
            erase(Seqs);
        T ->
            T
    end.

%% capx/1

capx(recv_CER) ->
    'CER';
capx({'Wait-CEA', _, _}) ->
    'CEA'.

%% start_next/1

start_next(#state{service = Svc0} = S) ->
    case getr(?Q_KEY) of
        {Addrs0, Tmo, Data} ->
            Svc = readdr(Svc0, Addrs0),
            {TPid, Addrs} = start_transport(Addrs0, {Svc, Tmo, Data}),
            S#state{transport = TPid,
                    service = svc(Svc, Addrs)};
        undefined ->
            stop
    end.

%% send_CER/1

send_CER(#state{state = {'Wait-Conn-Ack', Tmo},
                mode = {connect, Remote},
                service = #diameter_service{capabilities = LCaps},
                transport = TPid,
                dictionary = Dict,
                codec = Opts}
         = S) ->
    OH = LCaps#diameter_caps.origin_host,
    req_send_CER(OH, Remote)
        orelse
        close({already_connected, Remote, LCaps}),
    CER = build_CER(S),
    #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                               hop_by_hop_id = Hid}}
        = Pkt
        = encode(CER, Opts, Dict),
    incr(send, Pkt, Dict),
    send(TPid, Pkt),
    ?LOG(send, 'CER'),
    start_timer(Tmo, S#state{state = {'Wait-CEA', Hid, Eid}}).

%% Register ourselves as connecting to the remote endpoint in
%% question. This isn't strictly necessary since a peer implementing
%% the 3588 Peer State Machine should reject duplicate connection's
%% from the same peer but there's little point in us setting up a
%% duplicate connection in the first place. This could also include
%% the transport protocol being used but since we're blind to
%% transport just avoid duplicate connections to the same host/port.
req_send_CER(OriginHost, Remote) ->
    register_everywhere({?MODULE, connection, OriginHost, {remote, Remote}}).

%% start_timer/2

start_timer(Tmo, #state{state = PS} = S) ->
    erlang:send_after(Tmo, self(), {timeout, PS}),
    S.

%% build_CER/1

build_CER(#state{service = #diameter_service{capabilities = LCaps},
                 dictionary = Dict}) ->
    {ok, CER} = diameter_capx:build_CER(LCaps, Dict),
    CER.

%% encode/3

encode(Rec, Opts, Dict) ->
    Seq = diameter_session:sequence({_,_} = getr(?SEQUENCE_KEY)),
    Hdr = #diameter_header{version = ?DIAMETER_VERSION,
                           end_to_end_id = Seq,
                           hop_by_hop_id = Seq},
    diameter_codec:encode(Dict, Opts, #diameter_packet{header = Hdr,
                                                       msg = Rec}).

%% incoming/2

incoming(#diameter_header{is_request = R}, #state{transport = TPid,
                                                  ack = Ack}) ->
    R andalso Ack andalso send(TPid, false),
    ok;

incoming(<<_:32, 1:1, _/bits>>, #state{ack = true} = S) ->
    send(S#state.transport, false),
    ok;

incoming(<<_/bits>>, _) ->
    ok;

incoming(T, _) ->
    T.

%% recv/2

recv(#diameter_packet{bin = Bin} = Pkt, S) ->
    recv(Bin, Pkt, S);

recv(Bin, S) ->
    recv(Bin, Bin, S).

%% recv/3

recv(Bin, Msg, S) ->
    recv(diameter_codec:decode_header(Bin), Bin, Msg, S).

%% recv/4

recv(false, Bin, _, #state{length_errors = E}) ->
    invalid(E, truncated_header, Bin),
    Bin;

recv(#diameter_header{length = Len} = H, Bin, Msg, #state{length_errors = E,
                                                          incoming_maxlen = M,
                                                          dictionary = Dict0}
                                                   = S)
  when E == handle;
       0 == Len rem 4, bit_size(Bin) == 8*Len, size(Bin) =< M ->
    recv1(diameter_codec:msg_name(Dict0, H), H, Msg, S);

recv(H, Bin, _, #state{incoming_maxlen = M})
  when M < size(Bin) ->
    invalid(false, incoming_maxlen_exceeded, {size(Bin), H}),
    H;

recv(H, Bin, _, #state{length_errors = E}) ->
    T = {size(Bin), bit_size(Bin) rem 8, H},
    invalid(E, message_length_mismatch, T),
    H.

%% recv1/4

%% Ignore anything but an expected CER/CEA if so configured. This is
%% non-standard behaviour.
recv1(Name, H, _, #state{state = {'Wait-CEA', _, _},
                         strict = false})
  when Name /= 'CEA' ->
    H;
recv1(Name, H, _, #state{state = recv_CER,
                         strict = false})
  when Name /= 'CER' ->
    H;

%% Incoming request after outgoing DPR: discard. Don't discard DPR, so
%% both ends don't do so when sending simultaneously.
recv1(Name, #diameter_header{is_request = true} = H, _, #state{dpr = {_,_,_}})
  when Name /= 'DPR' ->
    invalid(false, recv_after_outgoing_dpr, H),
    H;

%% Incoming request after incoming DPR: discard.
recv1(_, #diameter_header{is_request = true} = H, _, #state{dpr = true}) ->
    invalid(false, recv_after_incoming_dpr, H),
    H;

%% DPA with identifier mismatch, or in response to a DPR initiated by
%% the service.
recv1('DPA' = Name,
      #diameter_header{hop_by_hop_id = Hid, end_to_end_id = Eid}
      = H,
      Msg,
      #state{dpr = {X,HI,EI}}
      = S)
  when HI /= Hid;
       EI /= Eid;
       not X ->
    Pkt = pkt(H, Msg),
    handle(Name, Pkt, S);

%% Any other message with a header and no length errors.
recv1(Name, H, Msg, #state{parent = Pid, ack = Ack} = S) ->
    Pkt = pkt(H, Msg),
    Pid ! {recv, self(), get_route(Ack, Name, Pkt), Name, Pkt},
    handle(Name, Pkt, S).

%% pkt/2

pkt(H, Bin)
  when is_binary(Bin) ->
    #diameter_packet{header = H,
                     bin = Bin};

pkt(H, Pkt) ->
    Pkt#diameter_packet{header = H}.

%% invalid/3

%% Note that counters here only count discarded messages.
invalid(E, Reason, T) ->
    diameter_stats:incr(Reason),
    E == exit andalso close({Reason, T}),
    ?LOG(Reason, T),
    ok.

%% handle/3

%% Incoming CEA.
handle('CEA' = N,
       #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                                  hop_by_hop_id = Hid}}
       = Pkt,
       #state{state = {'Wait-CEA', Hid, Eid}}
       = S) ->
    ?LOG(recv, N),
    handle_CEA(Pkt, S);

%% Incoming CER
handle('CER' = N, Pkt, #state{state = recv_CER} = S) ->
    handle_request(N, Pkt, S);

%% Anything but CER/CEA in a non-Open state is an error, as is
%% CER/CEA in anything but recv_CER/Wait-CEA.
handle(Name, _, #state{state = PS})
  when PS /= 'Open';
       Name == 'CER';
       Name == 'CEA' ->
    {stop, {Name, PS}};

handle('DPR' = N, Pkt, S) ->
    handle_request(N, Pkt, S);

%% DPA in response to DPR, with the expected identifiers.
handle('DPA' = N,
       #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                                  hop_by_hop_id = Hid}
                               = H}
       = Pkt,
    #state{dictionary = Dict0,
           transport = TPid,
           dpr = {X, Hid, Eid},
           codec = Opts}) ->
    ?LOG(recv, N),
    X orelse begin
                 %% Only count DPA in response to a DPR sent by the
                 %% service: explicit DPR is counted in the same way
                 %% as other explicitly sent requests.
                 incr(recv, H, Dict0),
                 {_, RecPkt} = decode(Dict0, Opts, Pkt),
                 incr_rc(recv, RecPkt, Dict0)
             end,
    diameter_peer:close(TPid),
    {stop, N};

%% Ignore an unsolicited DPA in particular. Note that dpa_timeout
%% deals with the case in which the peer sends the wrong identifiers
%% in DPA.
handle('DPA' = N, #diameter_packet{header = H}, _) ->
    ?LOG(ignored, N),
    %% Note that these aren't counted in the normal recv counter.
    diameter_stats:incr({diameter_codec:msg_id(H), recv, ignored}),
    ok;

handle(_, _, _) ->
    ok.

%% incr/3

incr(Dir, Hdr, Dict0) ->
    diameter_traffic:incr(Dir, Hdr, self(), Dict0).

%% incr_rc/3

incr_rc(Dir, Pkt, Dict0) ->
    diameter_traffic:incr_rc(Dir, Pkt, self(), Dict0).

%% incr_error/3

incr_error(Dir, Pkt, Dict0) ->
    diameter_traffic:incr_error(Dir, Pkt, self(), Dict0).

%% send/2

%% Msg here could be a #diameter_packet or a binary depending on who's
%% sending. In particular, the watchdog will send DWR as a binary
%% while messages coming from clients will be in a #diameter_packet.

send(Pid, Msg) ->
    diameter_peer:send(Pid, Msg).

%% outgoing/2

%% Explicit DPR.
outgoing(#diameter_packet{header = #diameter_header{application_id = 0,
                                                    cmd_code = 282,
                                                    is_request = true}
                                 = H}
         = Pkt,
         #state{dpr = T,
                parent = Pid}
         = S) ->
    if T == false ->
            inform_dpr(Pid),
            send_dpr(true, Pkt, dpa_timeout(), S);
       T == true ->
            invalid(false, dpr_after_dpa, H);  %% DPA sent: discard
       true ->
            invalid(false, dpr_after_dpr, H)   %% DPR sent: discard
    end;

%% Explicit CER or DWR: discard. These are sent by us.
outgoing(#diameter_packet{header = #diameter_header{application_id = 0,
                                                    cmd_code = C,
                                                    is_request = true}
                                 = H},
         _)
  when 257 == C;    %% CER
       280 == C ->  %% DWR
    invalid(false, invalid_request, H);

%% DPR not sent: send.
outgoing(Msg, #state{transport = TPid, dpr = false}) ->
    send(TPid, Msg),
    ok;

%% Outgoing answer: send.
outgoing(#diameter_packet{header = #diameter_header{is_request = false}}
         = Pkt,
         #state{transport = TPid}) ->
    send(TPid, Pkt),
    ok;

%% Outgoing request: discard.
outgoing(Msg, #state{dpr = {_,_,_}}) ->
    invalid(false, send_after_dpr, header(Msg)).

header(#diameter_packet{header = H}) ->
    H;
header(Bin) ->  %% DWR
    diameter_codec:decode_header(Bin).

%% handle_request/3
%%
%% Incoming CER or DPR.

handle_request(Name,
               #diameter_packet{header = H}
               = Pkt,
               #state{dictionary = Dict0,
                      codec = Opts}
               = S) ->
    ?LOG(recv, Name),
    incr(recv, H, Dict0),
    send_answer(Name, decode(Dict0, Opts, Pkt), S).

%% decode/3
%%
%% Decode the message as record for diameter_capx, and in the
%% configured format for events.

decode(Dict0, Opts, Pkt) ->
    {diameter_codec:decode(Dict0, Opts, Pkt),
     diameter_codec:decode(Dict0, Opts#{decode_format := record}, Pkt)}.

%% send_answer/3

send_answer(Type, {DecPkt, RecPkt}, #state{transport = TPid,
                                           dictionary = Dict,
                                           codec = Opts}
                                    = S) ->
    incr_error(recv, RecPkt, Dict),

    #diameter_packet{header = H,
                     transport_data = TD}
        = RecPkt,

    {Msg, PostF} = build_answer(Type, DecPkt, RecPkt, S),

    %% An answer message clears the R and T flags and retains the P
    %% flag. The E flag is set at encode.
    Pkt = #diameter_packet{header
                           = H#diameter_header{version = ?DIAMETER_VERSION,
                                               is_request = false,
                                               is_error = undefined,
                                               is_retransmitted = false},
                           msg = Msg,
                           transport_data = TD},

    AnsPkt = diameter_codec:encode(Dict, Opts, Pkt),

    incr(send, AnsPkt, Dict),
    incr_rc(send, AnsPkt, Dict),
    send(TPid, AnsPkt),
    ?LOG(send, ans(Type)),
    eval(PostF, S).

ans('CER') -> 'CEA';
ans('DPR') -> 'DPA'.

eval([F|A], S) ->
    apply(F, A ++ [S]);
eval(T, _) ->
    close(T).

%% build_answer/4

build_answer('CER',
             DecPkt,
             #diameter_packet{msg = CER,
                              header = #diameter_header{version
                                                        = ?DIAMETER_VERSION,
                                                        is_error = false},
                              errors = []},
             #state{dictionary = Dict0}
             = S) ->
    {SupportedApps, RCaps, CEA} = recv_CER(CER, S),

    [RC, IS] = Dict0:'#get-'(['Result-Code', 'Inband-Security-Id'], CEA),

    #diameter_caps{origin_host = {OH, DH}}
        = Caps
        = capz(caps(S), RCaps),

    try
        2001 == RC  %% DIAMETER_SUCCESS
            orelse ?THROW(RC),
        register_everywhere({?MODULE, connection, OH, DH})
            orelse ?THROW(4003),  %% DIAMETER_ELECTION_LOST
        caps_cb(Caps)
    of
        N -> {cea(CEA, N, Dict0), [fun open/5, DecPkt,
                                               SupportedApps,
                                               Caps,
                                               {accept, inband_security(IS)}]}
    catch
        ?FAILURE(Reason) ->
            rejected(Reason, {'CER', Reason, Caps, DecPkt}, S)
    end;

%% The error checks below are similar to those in diameter_traffic for
%% other messages. Should factor out the commonality.

build_answer(Type,
             DecPkt,
             #diameter_packet{header = H,
                              errors = Es},
             S) ->
    {RC, FailedAVP} = result_code(Type, H, Es),
    {answer(Type, RC, FailedAVP, S), post(Type, RC, DecPkt, S)}.

inband_security([]) ->
    ?NO_INBAND_SECURITY;
inband_security([IS]) ->
    IS.

cea(CEA, ok, _) ->
    CEA;
cea(CEA, 2001, _) ->
    CEA;
cea(CEA, RC, Dict0) ->
    Dict0:'#set-'({'Result-Code', RC}, CEA).

post('CER' = T, RC, Pkt, S) ->
    {T, caps(S), {RC, Pkt}};
post('DPR', _, _, #state{parent = Pid}) ->
    [fun(S) -> dpr_timer(), inform_dpr(Pid), dpr(S) end].

dpr(#state{dpr = false} = S) ->  %% not awaiting DPA
    S#state{dpr = true};  %% DPR received
dpr(S) ->  %% DPR already sent or received
    S.

inform_dpr(Pid) ->
    Pid ! {'DPR', self()}.  %% tell watchdog to die with us

rejected({capabilities_cb, _F, Reason}, T, S) ->
    rejected(Reason, T, S);

rejected(discard, T, _) ->
    close(T);
rejected({N, Es}, T, S) ->
    {answer('CER', N, failed_avp(N, Es), S), T};
rejected(N, T, S) ->
    {answer('CER', N, [], S), T}.

failed_avp(RC, [{RC, Avp} | _]) ->
    [{'Failed-AVP', [[{'AVP', [Avp]}]]}];
failed_avp(RC, [_ | Es]) ->
    failed_avp(RC, Es);
failed_avp(_, [] = No) ->
    No.

answer(Type, RC, FailedAVP, S) ->
    set(answer(Type, RC, S), FailedAVP).

answer(Type, RC, S) ->
    answer_message(answer(Type, S), RC).

%% answer_message/2

answer_message([_ | Avps], RC)
  when 3000 =< RC, RC < 4000 ->
    ['answer-message', {'Result-Code', RC}
                     | lists:filter(fun is_origin/1, Avps)];

answer_message(Msg, RC) ->
    Msg ++ [{'Result-Code', RC}].

is_origin({N, _}) ->
    N == 'Origin-Host'
        orelse N == 'Origin-Realm'
        orelse N == 'Origin-State-Id'.

%% set/2

set(Ans, []) ->
    Ans;
set(['answer-message' | _] = Ans, FailedAvp) ->
    Ans ++ [{'AVP', [FailedAvp]}];
set([_|_] = Ans, FailedAvp) ->
    Ans ++ FailedAvp.

%% result_code/3

%% Be lenient with errors in DPR since there's no reason to be
%% otherwise. Rejecting may cause the peer to missinterpret the error
%% as meaning that the connection should not be closed, which may well
%% lead to more problems than any errors in the DPR.

result_code('DPR', _, _) ->
    {2001, []};

result_code('CER', H, Es) ->
    result_code(H, Es).

%% result_code/2

result_code(#diameter_header{is_error = true}, _) ->
    {3008, []};  %% DIAMETER_INVALID_HDR_BITS

result_code(#diameter_header{version = ?DIAMETER_VERSION}, Es) ->
    rc(Es);

result_code(_, _) ->
    {5011, []}.  %% DIAMETER_UNSUPPORTED_VERSION

%% rc/1

rc([]) ->
    {2001, []};  %% DIAMETER_SUCCESS
rc([{RC, _} | _] = Es) ->
    {RC, failed_avp(RC, Es)};
rc([RC|_]) ->
    {RC, []}.

%%   DIAMETER_INVALID_HDR_BITS          3008
%%      A request was received whose bits in the Diameter header were
%%      either set to an invalid combination, or to a value that is
%%      inconsistent with the command code's definition.

%%   DIAMETER_INVALID_AVP_BITS          3009
%%      A request was received that included an AVP whose flag bits are
%%      set to an unrecognized value, or that is inconsistent with the
%%      AVP's definition.

%%   ELECTION_LOST                      4003
%%      The peer has determined that it has lost the election process and
%%      has therefore disconnected the transport connection.

%%   DIAMETER_NO_COMMON_APPLICATION     5010
%%      This error is returned when a CER message is received, and there
%%      are no common applications supported between the peers.

%%   DIAMETER_UNSUPPORTED_VERSION       5011
%%      This error is returned when a request was received, whose version
%%      number is unsupported.

%% answer/2

answer(Name, #state{service = #diameter_service{capabilities = Caps}}) ->
    a(Name, Caps).

a('CER', #diameter_caps{vendor_id = Vid,
                        origin_host = Host,
                        origin_realm = Realm,
                        host_ip_address = Addrs,
                        product_name = Name,
                        origin_state_id = OSI}) ->
    ['CEA', {'Origin-Host', Host},
            {'Origin-Realm', Realm},
            {'Host-IP-Address', Addrs},
            {'Vendor-Id', Vid},
            {'Product-Name', Name},
            {'Origin-State-Id', OSI}];

a('DPR', #diameter_caps{origin_host = {Host, _},
                        origin_realm = {Realm, _}}) ->
    ['DPA', {'Origin-Host', Host},
            {'Origin-Realm', Realm}].

%% recv_CER/2

recv_CER(CER, #state{service = Svc, dictionary = Dict}) ->
    case diameter_capx:recv_CER(CER, Svc, Dict) of
        {ok, T} ->
            T;
        {error, Reason} ->
            close({'CER', CER, Svc, Dict, Reason})
    end.

%% handle_CEA/2

handle_CEA(#diameter_packet{header = H}
           = Pkt,
           #state{dictionary = Dict0,
                  service = #diameter_service{capabilities = LCaps},
                  codec = Opts}
           = S) ->
    incr(recv, H, Dict0),

    {DecPkt, RecPkt} = decode(Dict0, Opts, Pkt),

    RC = result_code(incr_rc(recv, RecPkt, Dict0)),
    {SApps, IS, RCaps} = recv_CEA(RecPkt, S),

    #diameter_caps{origin_host = {OH, DH}}
        = Caps
        = capz(LCaps, RCaps),

    %% Ensure that we don't already have a connection to the peer in
    %% question. This isn't the peer election of 3588 except in the
    %% sense that, since we don't know who we're talking to until we
    %% receive a CER/CEA, the first that arrives wins the right to a
    %% connection with the peer.

    try
        is_integer(RC) andalso ?IS_SUCCESS(RC)
            orelse ?THROW(RC),
        [] == SApps
            andalso ?THROW(no_common_application),
        [] == IS
            andalso ?THROW(no_common_security),
        register_everywhere({?MODULE, connection, OH, DH})
            orelse ?THROW(election_lost),
        caps_cb(Caps)
    of
        _ -> open(DecPkt, SApps, Caps, {connect, hd([_] = IS)}, S)
    catch
        ?FAILURE(Reason) -> close({'CEA', Reason, Caps, DecPkt})
    end.
%% Check more than the result code since the peer could send success
%% regardless. If not 2001 then a peer_up callback could do anything
%% required. It's not unimaginable that a peer agreeing to TLS after
%% capabilities exchange could send DIAMETER_LIMITED_SUCCESS = 2002,
%% even if this isn't required by RFC 3588.

result_code({'Result-Code', N}) ->
    N;
result_code(_) ->
    undefined.

%% recv_CEA/2

recv_CEA(#diameter_packet{header = #diameter_header{version
                                                    = ?DIAMETER_VERSION,
                                                    is_error = false},
                          msg = CEA,
                          errors = []},
         #state{service = Svc,
                dictionary = Dict}) ->
    case diameter_capx:recv_CEA(CEA, Svc, Dict) of
        {ok, T} ->
            T;
        {error, Reason} ->
            close({'CEA', CEA, Svc, Dict, Reason})
    end;

recv_CEA(Pkt, S) ->
    close({'CEA', caps(S), Pkt}).

caps(#diameter_service{capabilities = Caps}) ->
    Caps;
caps(#state{service = Svc}) ->
    caps(Svc).

%% caps_cb/1

caps_cb(Caps) ->
    {Ref, Ts} = eraser(?CB_KEY),
    caps_cb(Ts, [Ref, Caps]).

caps_cb([], _) ->
    ok;
caps_cb([F | Rest], T) ->
    case diameter_lib:eval([F|T]) of
        ok ->
            caps_cb(Rest, T);
        N when ?IS_SUCCESS(N) ->  %% 2xxx result code: accept immediately
            N;
        Res ->
            ?THROW({capabilities_cb, F, rejected(Res)})
    end.
%% Note that returning 2xxx causes the capabilities exchange to be
%% accepted directly, without further callbacks.

rejected(discard = T) ->
    T;
rejected(unknown) ->
    3010;  %% DIAMETER_UNKNOWN_PEER
rejected(N)
  when is_integer(N) ->
    N.

%% open/5

open(Pkt, SupportedApps, Caps, {Type, IS}, #state{parent = Pid,
                                                  service = Svc}
                                           = S) ->
    #diameter_caps{origin_host = {_,_} = H,
                   inband_security_id = {LS,_}}
        = Caps,

    tls_ack(lists:member(?TLS, LS), Caps, Type, IS, S),
    Pid ! {open, self(), H, {Caps, SupportedApps, Pkt}},

    %% Replace capabilities record with local/remote pairs.
    S#state{state = 'Open',
            service = Svc#diameter_service{capabilities = Caps}}.

%% We've advertised TLS support: tell the transport the result
%% and expect a reply when the handshake is complete.
tls_ack(true, Caps, Type, IS, #state{transport = TPid}) ->
    Ref = make_ref(),
    TPid ! {diameter, {tls, Ref, Type, IS == ?TLS}},
    receive
        {diameter, {tls, Ref}} ->
            ok;
        {'DOWN', _, process, TPid, Reason} ->
            close({tls_ack, Reason, Caps})
    end;

%% Or not. Don't send anything to the transport so that transports
%% not supporting TLS work as before without modification.
tls_ack(false, _, _, _, _) ->
    ok.

capz(#diameter_caps{} = L, #diameter_caps{} = R) ->
    #diameter_caps{}
        = list_to_tuple([diameter_caps | lists:zip(tl(tuple_to_list(L)),
                                                   tl(tuple_to_list(R)))]).

%% close/1
%%
%% A good function to trace on in case of problems with capabilities
%% exchange.

close(Reason) ->
    throw({?MODULE, close, Reason}).

%% dpr/2
%%
%% The RFC isn't clear on whether DPR should be sent in a non-Open
%% state. The Peer State Machine transitions it documents aren't
%% exhaustive (no Stop in Wait-I-CEA for example) so assume it's up to
%% the implementation and transition to Closed (ie. die) if we haven't
%% yet reached Open.

%% Connection is open, DPR has not been sent.
dpr(Reason, #state{state = 'Open',
                   dpr = false,
                   service = #diameter_service{capabilities = Caps}}
            = S) ->
    CBs = getr(?DPR_KEY),
    Ref = getr(?REF_KEY),
    Peer = {self(), Caps},
    dpr(CBs, [Reason, Ref, Peer], S);

%% Connection is open, DPR already sent or received.
dpr(_, #state{state = 'Open'}) ->
    ok;

%% Connection not open.
dpr(_Reason, _S) ->
    stop.

%% dpr/3
%%
%% Note that an implementation that wants to do something
%% transport_module-specific can lookup the pid of the transport
%% process and contact it. (eg. diameter:service_info/2)

dpr([CB|Rest], [Reason | _] = Args, S) ->
    case diameter_lib:eval([CB | Args]) of
        {dpr, Opts} when is_list(Opts) ->
            send_dpr(Reason, Opts, S);
        dpr ->
            send_dpr(Reason, [], S);
        close = T ->
            {stop, {disconnect_cb, T}};
        ignore ->
            dpr(Rest, Args, S);
        T ->
            ?ERROR({disconnect_cb, CB, Args, T})
    end;

dpr([], [Reason | _], S) ->
    send_dpr(Reason, [], S).

-record(opts, {cause, timeout}).

send_dpr(Reason, DprOpts, #state{dictionary = Dict,
                                 service = #diameter_service{capabilities = Caps},
                                 codec = Opts}
                       = S) ->
    #opts{cause = Cause, timeout = Tmo}
        = lists:foldl(fun opt/2,
                      #opts{cause = case Reason of
                                        transport -> ?GOAWAY;
                                        _         -> ?REBOOT
                                    end,
                            timeout = dpa_timeout()},
                      DprOpts),
    #diameter_caps{origin_host = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,

    Pkt = encode(['DPR', {'Origin-Host', OH},
                         {'Origin-Realm', OR},
                         {'Disconnect-Cause', Cause}],
                 Opts,
                 Dict),
    send_dpr(false, Pkt, Tmo, S).

%% send_dpr/4

send_dpr(X,
         #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                                    hop_by_hop_id = Hid}}
         = Pkt,
         Tmo,
         #state{transport = TPid,
                dictionary = Dict}
         = S) ->
    %% Only count DPR sent by the service: explicit DPR is counted in
    %% the same way as other explicitly sent requests.
    X orelse incr(send, Pkt, Dict),
    send(TPid, Pkt),
    dpa_timer(Tmo),
    ?LOG(send, 'DPR'),
    S#state{dpr = {X, Hid, Eid}}.

%% opt/2

opt({timeout, Tmo}, Rec)
  when ?IS_TIMEOUT(Tmo) ->
    Rec#opts{timeout = Tmo};
opt({cause, Cause}, Rec)
  when ?IS_CAUSE(Cause) ->
    Rec#opts{cause = cause(Cause)};
opt(T, _) ->
    ?ERROR({invalid_option, T}).

cause(rebooting) -> ?REBOOT;
cause(goaway)    -> ?GOAWAY;
cause(busy)      -> ?BUSY;
cause(N)
  when ?IS_CAUSE(N) ->
    N;
cause(N) ->
    ?ERROR({invalid_cause, N}).

dpa_timer(Tmo) ->
    erlang:send_after(Tmo, self(), dpa_timeout).

dpa_timeout() ->
    {_, Tmo} = getr(?DPA_KEY),
    Tmo.

dpr_timer() ->
    dpa_timer(dpr_timeout()).

dpr_timeout() ->
    {Tmo, _} = getr(?DPA_KEY),
    Tmo.

%% register_everywhere/1
%%
%% Register a term and ensure it's not registered elsewhere. Note that
%% two process that simultaneously register the same term may well
%% both fail to do so this isn't foolproof.
%%
%% Everywhere is no longer everywhere, it's where a
%% restrict_connections service_opt() specifies.

register_everywhere(T) ->
    reg(getr(?RESTRICT_KEY), T).

reg(Nodes, T) ->
    add(lists:member(node(), Nodes), T) andalso unregistered(Nodes, T).

add(true, T) ->
    diameter_reg:add_new(T);
add(false, T) ->
    diameter_reg:add(T).

%% unregistered
%%
%% Ensure that the term in question isn't registered on other nodes.

unregistered(Nodes, T) ->
    {ResL, _} = rpc:multicall(Nodes, ?MODULE, match, [{node(), T}]),
    lists:all(fun nomatch/1, ResL).

nomatch({badrpc, {'EXIT', {undef, _}}}) ->  %% no diameter on remote node
    true;
nomatch(L) ->
    [] == L.

%% match/1

match({Node, _})
  when Node == node() ->
    [];
match({_, T}) ->
    try
        diameter_reg:match(T)
    catch
        _:_ -> []
    end.
