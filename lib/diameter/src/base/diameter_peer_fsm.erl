%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

%% RFC 3588:
%%
%%   Timeout        An application-defined timer has expired while waiting
%%                  for some event.
%%
-define(EVENT_TIMEOUT, 10000).
%% Default timeout for reception of CER/CEA.

%% Default timeout for DPA in response to DPR. A bit short but the
%% timeout used to be hardcoded. (So it could be worse.)
-define(DPA_TIMEOUT, 1000).

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
         service      :: #diameter_service{},
         dpr = false  :: false | {uint32(), uint32()},
                            %% | hop by hop and end to end identifiers
         length_errors :: exit | handle | discard}).

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

-spec start(T, [Opt], {diameter:sequence(),
                       [node()],
                       module(),
                       #diameter_service{}})
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

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

%% init/1

init(T) ->
    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], i(T)).

i({Ack, WPid, {M, Ref} = T, Opts, {Mask, Nodes, Dict0, Svc}}) ->
    erlang:monitor(process, WPid),
    wait(Ack, WPid),
    diameter_stats:reg(Ref),
    {[Cs,Ds], Rest} = proplists:split(Opts, [capabilities_cb, disconnect_cb]),
    putr(?CB_KEY, {Ref, [F || {_,F} <- Cs]}),
    putr(?DPR_KEY, [F || {_, F} <- Ds]),
    putr(?REF_KEY, Ref),
    putr(?SEQUENCE_KEY, Mask),
    putr(?RESTRICT_KEY, Nodes),

    Tmo = proplists:get_value(capx_timeout, Opts, ?EVENT_TIMEOUT),
    OnLengthErr = proplists:get_value(length_errors, Opts, exit),

    {TPid, Addrs} = start_transport(T, Rest, Svc),

    #state{state = {'Wait-Conn-Ack', Tmo},
           parent = WPid,
           transport = TPid,
           dictionary = Dict0,
           mode = M,
           service = svc(Svc, Addrs),
           length_errors = OnLengthErr}.
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
            exit({shutdown, D})
    end.

start_transport(T, Opts, #diameter_service{capabilities = LCaps} = Svc) ->
    Addrs0 = LCaps#diameter_caps.host_ip_address,
    start_transport(Addrs0, {T, Opts, Svc}).

start_transport(Addrs0, T) ->
    case diameter_peer:start(T) of
        {TPid, Addrs, Tmo, Data} ->
            erlang:monitor(process, TPid),
            q_next(TPid, Addrs0, Tmo, Data),
            {TPid, Addrs};
        No ->
            exit({shutdown, No})
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
            ?LOG(stop, T),
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
%% succesfully encoded. It's not checked at diameter:add_transport/2
%% since this can be called before creating the service.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

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
transition({diameter, {_, connected}}, _) ->
    {stop, connection_timeout};
transition({diameter, {_, connected, _}}, _) ->
    {stop, connection_timeout};
transition({diameter, {_, connected, _, _}}, _) ->
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

%% Incoming message from the transport.
transition({diameter, {recv, Pkt}}, S) ->
    recv(Pkt, S);

%% Timeout when still in the same state ...
transition({timeout = T, PS}, #state{state = PS}) ->
    {stop, {capx(PS), T}};

%% ... or not.
transition({timeout, _}, _) ->
    ok;

%% Outgoing message.
transition({send, Msg}, #state{transport = TPid}) ->
    send(TPid, Msg),
    ok;

%% Request for graceful shutdown at remove_transport, stop_service of
%% application shutdown.
transition({shutdown, Pid, Reason}, #state{parent = Pid, dpr = false} = S) ->
    dpr(Reason, S);
transition({shutdown, Pid, _}, #state{parent = Pid}) ->
    ok;

%% DPA reception has timed out.
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
    start_next(S);

%% Transport has died after connection timeout.
transition({'DOWN', _, process, _, _}, _) ->
    ok;

%% State query.
transition({state, Pid}, #state{state = S, transport = TPid}) ->
    Pid ! {self(), [S, TPid]},
    ok.

%% Crash on anything unexpected.

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
                dictionary = Dict}
         = S) ->
    OH = LCaps#diameter_caps.origin_host,
    req_send_CER(OH, Remote)
        orelse
        close({already_connected, Remote, LCaps}),
    CER = build_CER(S),
    #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                               hop_by_hop_id = Hid}}
        = Pkt
        = encode(CER, Dict),
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

%% encode/2

encode(Rec, Dict) ->
    Seq = diameter_session:sequence({_,_} = getr(?SEQUENCE_KEY)),
    Hdr = #diameter_header{version = ?DIAMETER_VERSION,
                           end_to_end_id = Seq,
                           hop_by_hop_id = Seq},
    diameter_codec:encode(Dict, #diameter_packet{header = Hdr,
                                                 msg = Rec}).

%% recv/2

recv(#diameter_packet{header = #diameter_header{} = Hdr}
     = Pkt,
     #state{parent = Pid,
            dictionary = Dict0}
     = S) ->
    Name = diameter_codec:msg_name(Dict0, Hdr),
    Pid ! {recv, self(), Name, Pkt},
    rcv(Name, Pkt, S);

recv(#diameter_packet{header = undefined,
                      bin = Bin}
     = Pkt,
     S) ->
    recv(diameter_codec:decode_header(Bin), Pkt, S);

recv(Bin, S) ->
    recv(#diameter_packet{bin = Bin}, S).

%% recv/3

recv(#diameter_header{length = Len}
     = H,
     #diameter_packet{bin = Bin}
     = Pkt,
     #state{length_errors = E}
     = S)
  when E == handle;
       0 == Len rem 4, bit_size(Bin) == 8*Len ->
    recv(Pkt#diameter_packet{header = H}, S);

recv(#diameter_header{}
     = H,
     #diameter_packet{bin = Bin},
     #state{length_errors = E}) ->
    T = {size(Bin), bit_size(Bin) rem 8, H},
    invalid(E, message_length_mismatch, T);

recv(false, #diameter_packet{bin = Bin}, #state{length_errors = E}) ->
    invalid(E, truncated_header, Bin).

%% Note that counters here only count discarded messages.
invalid(E, Reason, T) ->
    diameter_stats:incr(Reason),
    E == exit andalso close({Reason, T}),
    ?LOG(Reason, T),
    ok.

%% rcv/3

%% Incoming CEA.
rcv('CEA' = N,
    #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                               hop_by_hop_id = Hid}}
    = Pkt,
    #state{state = {'Wait-CEA', Hid, Eid}}
    = S) ->
    ?LOG(recv, N),
    handle_CEA(Pkt, S);

%% Incoming CER
rcv('CER' = N, Pkt, #state{state = recv_CER} = S) ->
    handle_request(N, Pkt, S);

%% Anything but CER/CEA in a non-Open state is an error, as is
%% CER/CEA in anything but recv_CER/Wait-CEA.
rcv(Name, _, #state{state = PS})
  when PS /= 'Open';
       Name == 'CER';
       Name == 'CEA' ->
    {stop, {Name, PS}};

rcv('DPR' = N, Pkt, S) ->
    handle_request(N, Pkt, S);

%% DPA in response to DPR and with the expected identifiers.
rcv('DPA' = N,
    #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                               hop_by_hop_id = Hid}
                            = H}
    = Pkt,
    #state{dictionary = Dict0,
           transport = TPid,
           dpr = {Hid, Eid}}) ->
    ?LOG(recv, N),
    incr(recv, H, Dict0),
    incr_rc(recv, diameter_codec:decode(Dict0, Pkt), Dict0),
    diameter_peer:close(TPid),
    {stop, N};

%% Ignore anything else, an unsolicited DPA in particular.
rcv(N, #diameter_packet{header = H}, _)
  when N == 'CER';
       N == 'CEA';
       N == 'DPR';
       N == 'DPA' ->
    ?LOG(ignored, N),
    %% Note that these aren't counted in the normal recv counter.
    diameter_stats:incr({diameter_codec:msg_id(H), recv, ignored}),
    ok;

rcv(_, _, _) ->
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

%% handle_request/3
%%
%% Incoming CER or DPR.

handle_request(Name,
               #diameter_packet{header = H} = Pkt,
               #state{dictionary = Dict0} = S) ->
    ?LOG(recv, Name),
    incr(recv, H, Dict0),
    send_answer(Name, diameter_codec:decode(Dict0, Pkt), S).

%% send_answer/3

send_answer(Type, ReqPkt, #state{transport = TPid, dictionary = Dict} = S) ->
    incr_error(recv, ReqPkt, Dict),

    #diameter_packet{header = H,
                     transport_data = TD}
        = ReqPkt,

    {Msg, PostF} = build_answer(Type, ReqPkt, S),

    %% An answer message clears the R and T flags and retains the P
    %% flag. The E flag is set at encode.
    Pkt = #diameter_packet{header
                           = H#diameter_header{version = ?DIAMETER_VERSION,
                                               is_request = false,
                                               is_error = undefined,
                                               is_retransmitted = false},
                           msg = Msg,
                           transport_data = TD},

    AnsPkt = diameter_codec:encode(Dict, Pkt),

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

%% build_answer/3

build_answer('CER',
             #diameter_packet{msg = CER,
                              header = #diameter_header{version
                                                        = ?DIAMETER_VERSION,
                                                        is_error = false},
                              errors = []}
             = Pkt,
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
        N -> {cea(CEA, N, Dict0), [fun open/5, Pkt,
                                               SupportedApps,
                                               Caps,
                                               {accept, inband_security(IS)}]}
    catch
        ?FAILURE(Reason) ->
            rejected(Reason, {'CER', Reason, Caps, Pkt}, S)
    end;

%% The error checks below are similar to those in diameter_traffic for
%% other messages. Should factor out the commonality.

build_answer(Type,
             #diameter_packet{header = H,
                              errors = Es}
             = Pkt,
             S) ->
    {RC, FailedAVP} = result_code(H, Es),
    {answer(Type, RC, FailedAVP, S), post(Type, RC, Pkt, S)}.

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
post('DPR' = T, _, _, #state{parent = Pid}) ->
    [fun(S) -> Pid ! {T, self()}, S end].

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
                  service = #diameter_service{capabilities = LCaps}}
           = S) ->
    incr(recv, H, Dict0),

    #diameter_packet{}
        = DPkt
        = diameter_codec:decode(Dict0, Pkt),

    RC = result_code(incr_rc(recv, DPkt, Dict0)),

    {SApps, IS, RCaps} = recv_CEA(DPkt, S),

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
        _ -> open(DPkt, SApps, Caps, {connect, hd([_] = IS)}, S)
    catch
        ?FAILURE(Reason) -> close({'CEA', Reason, Caps, DPkt})
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
%% The RFC isn't clear on whether DPR should be send in a non-Open
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

%% Connection is open, DPR already sent.
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

-record(opts, {cause, timeout = ?DPA_TIMEOUT}).

send_dpr(Reason, Opts, #state{transport = TPid,
                              dictionary = Dict,
                              service = #diameter_service{capabilities = Caps}}
                       = S) ->
    #opts{cause = Cause, timeout = Tmo}
        = lists:foldl(fun opt/2,
                      #opts{cause = case Reason of
                                        transport -> ?GOAWAY;
                                        _         -> ?REBOOT
                                    end,
                            timeout = ?DPA_TIMEOUT},
                      Opts),
    #diameter_caps{origin_host = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,

    #diameter_packet{header = #diameter_header{end_to_end_id = Eid,
                                               hop_by_hop_id = Hid}}
        = Pkt
        = encode(['DPR', {'Origin-Host', OH},
                         {'Origin-Realm', OR},
                         {'Disconnect-Cause', Cause}],
                 Dict),
    incr(send, Pkt, Dict),
    send(TPid, Pkt),
    dpa_timer(Tmo),
    ?LOG(send, 'DPR'),
    S#state{dpr = {Hid, Eid}}.

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
