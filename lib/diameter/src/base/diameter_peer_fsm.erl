%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% This module implements (as a process) the RFC 3588 Peer State
%% Machine modulo the necessity of adapting the peer election to the
%% fact that we don't know the identity of a peer until we've
%% received a CER/CEA from it.
%%

-module(diameter_peer_fsm).
-behaviour(gen_server).

%% Interface towards diameter_watchdog.
-export([start/3]).

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
-include("diameter_types.hrl").
-include("diameter_gen_base_rfc3588.hrl").

-define(GOAWAY, ?'DIAMETER_BASE_DISCONNECT-CAUSE_DO_NOT_WANT_TO_TALK_TO_YOU').
-define(REBOOT, ?'DIAMETER_BASE_DISCONNECT-CAUSE_REBOOTING').

-define(NO_INBAND_SECURITY, 0).
-define(TLS, 1).

-define(LOOP_TIMEOUT, 2000).

%% RFC 3588:
%%
%%   Timeout        An application-defined timer has expired while waiting
%%                  for some event.
%%
-define(EVENT_TIMEOUT, 10000).

%% How long to wait for a DPA in response to DPR before simply
%% aborting. Used to distinguish between shutdown and not but there's
%% not really any need. Stopping a service will require a timeout if
%% the peer doesn't answer DPR so the value should be short-ish.
-define(DPA_TIMEOUT, 1000).

-record(state,
        {state = 'Wait-Conn-Ack'   %% state of RFC 3588 Peer State Machine
              :: 'Wait-Conn-Ack' | recv_CER | 'Wait-CEA' | 'Open',
         mode :: accept | connect | {connect, reference()},
         parent       :: pid(),
         transport    :: pid(),
         service      :: #diameter_service{},
         dpr = false  :: false | {'Unsigned32'(), 'Unsigned32'()}}).
                            %% | hop by hop and end to end identifiers

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
%% below to start the process implementing the Peer State Machine. The
%% former is a "peer" in diameter_service while the latter is a
%% "conn". In a sense, diameter_service sees the watchdog as
%% implementing the Peer State Machine and the process implemented
%% here as being the transport, not being aware of the watchdog at
%% all.
%%

%%% ---------------------------------------------------------------------------
%%% # start({connect|accept, Ref}, Opts, Service)
%%%
%%% Output: Pid
%%% ---------------------------------------------------------------------------

%% diameter_config requires a non-empty list of applications on the
%% service but diameter_service then constrains the list to any
%% specified on the transport in question. Check here that the list is
%% still non-empty.

start({_, Ref} = Type, Opts, #diameter_service{applications = Apps} = Svc) ->
    [] /= Apps orelse ?ERROR({no_apps, Type, Opts}),
    T = {self(), Type, Opts, Svc},
    {ok, Pid} = diameter_peer_fsm_sup:start_child(T),
    diameter_stats:reg(Pid, Ref),
    Pid.

start_link(T) ->
    {ok, _} = proc_lib:start_link(?MODULE,
                                  init,
                                  [T],
                                  infinity,
                                  diameter_lib:spawn_opts(server, [])).

%%% ---------------------------------------------------------------------------
%%% ---------------------------------------------------------------------------

%% init/1

init(T) ->
    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], i(T)).

i({WPid, {M, _} = T, Opts, #diameter_service{capabilities = Caps} = Svc0}) ->
    putr(dwa, dwa(Caps)),
    {ok, TPid, Svc} = start_transport(T, Opts, Svc0),
    erlang:monitor(process, TPid),
    erlang:monitor(process, WPid),
    #state{parent = WPid,
           transport = TPid,
           mode = M,
           service = Svc}.
%% The transport returns its local ip addresses so that different
%% transports on the same service can use different local addresses.
%% The local addresses are put into Host-IP-Address avps here when
%% sending capabilities exchange messages.
%%
%% Invalid transport config may cause us to crash but note that the
%% watchdog start (start/2) succeeds regardless so as not to crash the
%% service.

start_transport(T, Opts, Svc) ->
    case diameter_peer:start(T, Opts, Svc) of
        {ok, TPid} ->
            {ok, TPid, Svc};
        {ok, TPid, [_|_] = Addrs} ->
            #diameter_service{capabilities = Caps0} = Svc,
            Caps = Caps0#diameter_caps{host_ip_address = Addrs},
            {ok, TPid, Svc#diameter_service{capabilities = Caps}};
        No ->
            exit({shutdown, No})
    end.

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
            ?LOGC(X =/= State#state.state, transition, X),
            {noreply, S};
        {stop, Reason} ->
            ?LOG(stop, Reason),
            x(Reason, State);
        stop ->
            ?LOG(stop, T),
            x(T, State)
    catch
        throw: {?MODULE, Tag, Reason}  ->
            ?LOG(Tag, {Reason, T}),
            {stop, {shutdown, Reason}, State}
    end.

x(Reason, #state{} = S) ->
    close_wd(Reason, S),
    {stop, {shutdown, Reason}, S}.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

code_change(_, State, _) ->
    {ok, State}.

%%% ---------------------------------------------------------------------------
%%% ---------------------------------------------------------------------------

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

%% transition/2

%% Connection to peer.
transition({diameter, {TPid, connected, Remote}},
           #state{state = PS,
                  mode = M}
           = S) ->
    'Wait-Conn-Ack' = PS,  %% assert
    connect = M,           %%
    send_CER(S#state{mode = {M, Remote},
                     transport = TPid});

%% Connection from peer.
transition({diameter, {TPid, connected}},
           #state{state = PS,
                  mode = M,
                  parent = Pid}
           = S) ->
    'Wait-Conn-Ack' = PS,  %% assert
    accept = M,            %%
    Pid ! {accepted, self()},
    start_timer(S#state{state = recv_CER,
                        transport = TPid});

%% Incoming message from the transport.
transition({diameter, {recv, Pkt}}, S) ->
    recv(Pkt, S);

%% Timeout when still in the same state ...
transition({timeout, PS}, #state{state = PS}) ->
    stop;

%% ... or not.
transition({timeout, _}, _) ->
    ok;

%% Outgoing message.
transition({send, Msg}, #state{transport = TPid}) ->
    send(TPid, Msg),
    ok;

%% Request for graceful shutdown.
transition({shutdown, Pid}, #state{parent = Pid, dpr = false} = S) ->
    dpr(?GOAWAY, S);
transition({shutdown, Pid}, #state{parent = Pid}) ->
    ok;

%% Application shutdown.
transition(shutdown, #state{dpr = false} = S) ->
    dpr(?REBOOT, S);
transition(shutdown, _) ->  %% DPR already send: ensure expected timeout
    dpa_timer(),
    ok;

%% Request to close the transport connection.
transition({close = T, Pid}, #state{parent = Pid,
                                    transport = TPid}) ->
    diameter_peer:close(TPid),
    {stop, T};

%% DPA reception has timed out.
transition(dpa_timeout, _) ->
    stop;

%% Someone wants to know a resolved port: forward to the transport process.
transition({resolve_port, _Pid} = T, #state{transport = TPid}) ->
    TPid ! T,
    ok;

%% Parent or transport has died.
transition({'DOWN', _, process, P, _},
           #state{parent = Pid,
                  transport = TPid})
  when P == Pid;
       P == TPid ->
    stop;

%% State query.
transition({state, Pid}, #state{state = S, transport = TPid}) ->
    Pid ! {self(), [S, TPid]},
    ok.

%% Crash on anything unexpected.

%% send_CER/1

send_CER(#state{mode = {connect, Remote},
                service = #diameter_service{capabilities = Caps},
                transport = TPid}
         = S) ->
    req_send_CER(Caps#diameter_caps.origin_host, Remote)
        orelse
        close(connected, S),
    CER = build_CER(S),
    ?LOG(send, 'CER'),
    send(TPid, encode(CER)),
    start_timer(S#state{state = 'Wait-CEA'}).

%% Register ourselves as connecting to the remote endpoint in
%% question. This isn't strictly necessary since a peer implementing
%% the 3588 Peer State Machine should reject duplicate connection's
%% from the same peer but there's little point in us setting up a
%% duplicate connection in the first place. This could also include
%% the transport protocol being used but since we're blind to
%% transport just avoid duplicate connections to the same host/port.
req_send_CER(OriginHost, Remote) ->
    register_everywhere({?MODULE, connection, OriginHost, {remote, Remote}}).

%% start_timer/1

start_timer(#state{state = PS} = S) ->
    erlang:send_after(?EVENT_TIMEOUT, self(), {timeout, PS}),
    S.

%% build_CER/1

build_CER(#state{service = #diameter_service{capabilities = Caps}}) ->
    {ok, CER} = diameter_capx:build_CER(Caps),
    CER.

%% encode/1

encode(Rec) ->
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Rec),
    Bin.

%% recv/2

%% RFC 3588 has result code 5015 for an invalid length but if a
%% transport is detecting message boundaries using the length header
%% then a length error will likely lead to further errors.

recv(#diameter_packet{header = #diameter_header{length = Len}
                             = Hdr,
                      bin = Bin},
     S)
  when Len < 20;
       (0 /= Len rem 4 orelse bit_size(Bin) /= 8*Len) ->
    discard(invalid_message_length, recv, [size(Bin),
                                           bit_size(Bin) rem 8,
                                           Hdr,
                                           S]);

recv(#diameter_packet{header = #diameter_header{} = Hdr}
     = Pkt,
     #state{parent = Pid}
     = S) ->
    Name = diameter_codec:msg_name(Hdr),
    Pid ! {recv, self(), Name, Pkt},
    diameter_stats:incr({msg_id(Name, Hdr), recv}), %% count received
    rcv(Name, Pkt, S);

recv(#diameter_packet{header = undefined,
                      bin = Bin}
     = Pkt,
     S) ->
    recv(Pkt#diameter_packet{header = diameter_codec:decode_header(Bin)}, S);

recv(Bin, S)
  when is_binary(Bin) ->
    recv(#diameter_packet{bin = Bin}, S);

recv(#diameter_packet{header = false} = Pkt, S) ->
    discard(truncated_header, recv, [Pkt, S]).

msg_id({_,_,_} = T, _) ->
    T;
msg_id(_, Hdr) ->
    diameter_codec:msg_id(Hdr).

%% Treat invalid length as a transport error and die. Especially in
%% the TCP case, in which there's no telling where the next message
%% begins in the incoming byte stream, keeping a crippled connection
%% alive may just make things worse.

discard(Reason, F, A) ->
    diameter_stats:incr(Reason),
    diameter_lib:warning_report(Reason, {?MODULE, F, A}),
    throw({?MODULE, abort, Reason}).

%% rcv/3

%% Incoming CEA.
rcv('CEA', Pkt, #state{state = 'Wait-CEA'} = S) ->
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

rcv(N, Pkt, S)
  when N == 'DWR';
       N == 'DPR' ->
    handle_request(N, Pkt, S);

%% DPA even though we haven't sent DPR: ignore.
rcv('DPA', _Pkt, #state{dpr = false}) ->
    ok;

%% DPA in response to DPR. We could check the sequence numbers but
%% don't bother, just close.
rcv('DPA' = N, _Pkt, #state{transport = TPid}) ->
    diameter_peer:close(TPid),
    {stop, N};

rcv(_, _, _) ->
    ok.

%% send/2

%% Msg here could be a #diameter_packet or a binary depending on who's
%% sending. In particular, the watchdog will send DWR as a binary
%% while messages coming from clients will be in a #diameter_packet.
send(Pid, Msg) ->
    diameter_stats:incr({diameter_codec:msg_id(Msg), send}),
    diameter_peer:send(Pid, Msg).

%% handle_request/3

handle_request(Type, #diameter_packet{} = Pkt, S) ->
    ?LOG(recv, Type),
    send_answer(Type, diameter_codec:decode(?BASE, Pkt), S).

%% send_answer/3

send_answer(Type, ReqPkt, #state{transport = TPid} = S) ->
    #diameter_packet{header = #diameter_header{version = V,
                                               end_to_end_id = Eid,
                                               hop_by_hop_id = Hid,
                                               is_proxiable = P},
                     transport_data = TD}
        = ReqPkt,

    {Answer, PostF} = build_answer(Type, V, ReqPkt, S),

    Pkt = #diameter_packet{header = #diameter_header{version = V,
                                                     end_to_end_id = Eid,
                                                     hop_by_hop_id = Hid,
                                                     is_proxiable = P},
                           msg = Answer,
                           transport_data = TD},

    send(TPid, diameter_codec:encode(?BASE, Pkt)),
    eval(PostF, S).

eval([F|A], S) ->
    apply(F, A ++ [S]);
eval(ok, S) ->
    S.

%% build_answer/4

build_answer('CER',
             ?DIAMETER_VERSION,
             #diameter_packet{msg = CER,
                              header = #diameter_header{is_error = false},
                              errors = []}
             = Pkt,
             #state{service = Svc}
             = S) ->
    #diameter_service{capabilities = #diameter_caps{origin_host = OH}}
        = Svc,

    {SupportedApps,
     #diameter_caps{origin_host = DH} = RCaps,
     #diameter_base_CEA{'Result-Code' = RC}
     = CEA}
        = recv_CER(CER, S),

    try
        2001 == RC  %% DIAMETER_SUCCESS
            orelse ?THROW({sent_CEA, RC}),
        register_everywhere({?MODULE, connection, OH, DH})
            orelse ?THROW({election_lost, 4003}),
        #diameter_base_CEA{'Inband-Security-Id' = [IS]}
            = CEA,
        {CEA, [fun open/5, Pkt, SupportedApps, RCaps, {accept, IS}]}
    catch
        ?FAILURE({Reason, RC}) ->
            {answer('CER', S) ++ [{'Result-Code', RC}],
             [fun close/2, {'CER', Reason, DH}]}
    end;

%% The error checks below are similar to those in diameter_service for
%% other messages. Should factor out the commonality.

build_answer(Type, V, #diameter_packet{header = H, errors = Es} = Pkt, S) ->
    FailedAvp = failed_avp([A || {_,A} <- Es]),
    Ans = answer(answer(Type, S), V, H, Es),
    {set(Ans, FailedAvp), if 'CER' == Type ->
                                  [fun close/2, {Type, V, Pkt}];
                             true ->
                                  ok
                          end}.

failed_avp([] = No) ->
    No;
failed_avp(Avps) ->
    [{'Failed-AVP', [[{'AVP', Avps}]]}].

set(Ans, []) ->
    Ans;
set(['answer-message' | _] = Ans, FailedAvp) ->
    Ans ++ [{'AVP', [FailedAvp]}];
set([_|_] = Ans, FailedAvp) ->
    Ans ++ FailedAvp.

answer([_, OH, OR | _], _, #diameter_header{is_error = true}, _) ->
    ['answer-message', OH, OR, {'Result-Code', 3008}];

answer([_, OH, OR | _], _, _, [Bs|_])
  when is_bitstring(Bs) ->
    ['answer-message', OH, OR, {'Result-Code', 3009}];

answer(Ans, ?DIAMETER_VERSION, _, Es) ->
    Ans ++ [{'Result-Code', rc(Es)}];

answer(Ans, _, _, _) ->
    Ans ++ [{'Result-Code', 5011}].  %% DIAMETER_UNSUPPORTED_VERSION

rc([]) ->
    2001;  %% DIAMETER_SUCCESS
rc([{RC,_}|_]) ->
    RC;
rc([RC|_]) ->
    RC.

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

answer('DWR', _) ->
    getr(dwa);

answer(Name, #state{service = #diameter_service{capabilities = Caps}}) ->
    a(Name, Caps).

a('CER', #diameter_caps{vendor_id = Vid,
                        origin_host = Host,
                        origin_realm = Realm,
                        host_ip_address = Addrs,
                        product_name = Name}) ->
    ['CEA', {'Origin-Host', Host},
            {'Origin-Realm', Realm},
            {'Host-IP-Address', Addrs},
            {'Vendor-Id', Vid},
            {'Product-Name', Name}];

a('DPR', #diameter_caps{origin_host = Host,
                        origin_realm = Realm}) ->
    ['DPA', {'Origin-Host', Host},
            {'Origin-Realm', Realm}].

%% recv_CER/2

recv_CER(CER, #state{service = Svc}) ->
    {ok, T} = diameter_capx:recv_CER(CER, Svc),
    T.

%% handle_CEA/1

handle_CEA(#diameter_packet{header = #diameter_header{version = V},
                            bin = Bin}
           = Pkt,
           #state{service = #diameter_service{capabilities = LCaps}}
           = S)
  when is_binary(Bin) ->
    ?LOG(recv, 'CEA'),

    ?DIAMETER_VERSION == V orelse close({version, V}, S),

    #diameter_packet{msg = CEA, errors = Errors}
        = DPkt
        = diameter_codec:decode(?BASE, Pkt),

    [] == Errors orelse close({errors, Errors}, S),

    {SApps, [IS], #diameter_caps{origin_host = DH} = RCaps}
        = recv_CEA(CEA, S),

    #diameter_caps{origin_host = OH}
        = LCaps,

    %% Ensure that we don't already have a connection to the peer in
    %% question. This isn't the peer election of 3588 except in the
    %% sense that, since we don't know who we're talking to until we
    %% receive a CER/CEA, the first that arrives wins the right to a
    %% connection with the peer.

    register_everywhere({?MODULE, connection, OH, DH})
        orelse close({'CEA', DH}, S),

    open(DPkt, SApps, RCaps, {connect, IS}, S).

%% recv_CEA/2

recv_CEA(CEA, #state{service = Svc} = S) ->
    case diameter_capx:recv_CEA(CEA, Svc) of
        {ok, {_,_}} -> %% return from old code
            close({'CEA', update}, S);
        {ok, {[], _, _}} ->
            close({'CEA', no_common_application}, S);
        {ok, {_, [], _}} ->
            close({'CEA', no_common_security}, S);
        {ok, {_,_,_} = T} ->
            T;
        {error, Reason} ->
            close({'CEA', Reason}, S)
    end.

%% open/5

open(Pkt, SupportedApps, RCaps, {Type, IS}, #state{parent = Pid,
                                                   service = Svc}
                                            = S) ->
    #diameter_service{capabilities = #diameter_caps{origin_host = OH,
                                                    inband_security_id = LS}
                                   = LCaps}
        = Svc,
    #diameter_caps{origin_host = DH}
        = RCaps,

    tls_ack(lists:member(?TLS, LS), Type, IS, S),
    Pid ! {open, self(), {OH,DH}, {capz(LCaps, RCaps), SupportedApps, Pkt}},

    S#state{state = 'Open'}.

%% We've advertised TLS support: tell the transport the result
%% and expect a reply when the handshake is complete.
tls_ack(true, Type, IS, #state{transport = TPid} = S) ->
    Ref = make_ref(),
    MRef = erlang:monitor(process, TPid),
    TPid ! {diameter, {tls, Ref, Type, IS == ?TLS}},
    receive
        {diameter, {tls, Ref}} ->
            erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, _, _} = T ->
            close({tls_ack, T}, S)
    end;

%% Or not. Don't send anything to the transport so that transports
%% not supporting TLS work as before without modification.
tls_ack(false, _, _, _) ->
    ok.

capz(#diameter_caps{} = L, #diameter_caps{} = R) ->
    #diameter_caps{}
        = list_to_tuple([diameter_caps | lists:zip(tl(tuple_to_list(L)),
                                                   tl(tuple_to_list(R)))]).

%% close/2

%% Tell the watchdog that our death isn't due to transport failure.
close(Reason, #state{parent = Pid}) ->
    close_wd(Reason, Pid),
    throw({?MODULE, close, Reason}).

%% close_wd/2

%% Ensure the watchdog dies if DPR has been sent ...
close_wd(_, #state{dpr = false}) ->
    ok;
close_wd(Reason, #state{parent = Pid}) ->
    close_wd(Reason, Pid);

%% ... or otherwise
close_wd(Reason, Pid) ->
    Pid ! {close, self(), Reason}.

%% dwa/1

dwa(#diameter_caps{origin_host = OH,
                   origin_realm = OR,
                   origin_state_id = OSI}) ->
    ['DWA', {'Origin-Host', OH},
            {'Origin-Realm', OR},
            {'Origin-State-Id', OSI}].

%% dpr/2

dpr(Cause, #state{transport = TPid,
                  service = #diameter_service{capabilities = Caps}}
           = S) ->
    #diameter_caps{origin_host = OH,
                   origin_realm = OR}
        = Caps,

    Bin = encode(['DPR', {'Origin-Host', OH},
                         {'Origin-Realm', OR},
                         {'Disconnect-Cause', Cause}]),
    send(TPid, Bin),
    dpa_timer(),
    ?LOG(send, 'DPR'),
    S#state{dpr = diameter_codec:sequence_numbers(Bin)}.

dpa_timer() ->
    erlang:send_after(?DPA_TIMEOUT, self(), dpa_timeout).

%% register_everywhere/1
%%
%% Register a term and ensure it's not registered elsewhere. Note that
%% two process that simultaneously register the same term may well
%% both fail to do so this isn't foolproof.

register_everywhere(T) ->
    diameter_reg:add_new(T)
        andalso unregistered(T).

unregistered(T) ->
    {ResL, _} = rpc:multicall(?MODULE, match, [{node(), T}]),
    lists:all(fun(L) -> [] == L end, ResL).

match({Node, _})
  when Node == node() ->
    [];
match({_, T}) ->
    try
        diameter_reg:match(T)
    catch
        _:_ -> []
    end.
