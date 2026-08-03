%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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
%% Tests of diameter_tcp/sctp as implementations of the diameter
%% transport interface.
%%

-module(diameter_transport_SUITE).

%% all tests, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
        
         %% The test cases
         parallel/1
        ]).

-export([accept/1,
         connect/1,
         reconnect/1,
         init/2]).

-export([message_client/4,
         message_server/4]).

-include_lib("kernel/include/inet_sctp.hrl").
-include("diameter.hrl").

-include("diameter_util.hrl").

%% Corresponding to diameter_* transport modules.
-define(TRANSPORTS, [tcp, sctp]).

%% Receive a message.
-define(RECV(Pat, Ret), receive Pat -> Ret end).
-define(RECV(Pat), ?RECV(Pat, diameter_lib:now())).

%% Sockets are opened on the loopback address.
-define(ADDR, {127,0,0,1}).

%% diameter_tcp doesn't use anything but host_ip_address, and that
%% only is a local address isn't configured as at transport start.
-define(SVC(Addrs), #diameter_service{capabilities
                                      = #diameter_caps{host_ip_address
                                                       = Addrs}}).

%% The term we register after open a listening port with gen_{tcp,sctp}.
-define(TEST_LISTENER(Ref, PortNr),
        {?MODULE, listen, Ref, PortNr}).

%% Message over the transport interface.
-define(TMSG(T), {diameter, T}).

%% Options for gen_tcp/gen_sctp.
-define(TCP_OPTS,  [binary, {active, true}, {packet, 0}]).
-define(SCTP_OPTS, [binary, {active, true}, {sctp_initmsg, ?SCTP_INIT}]).

%% Request a specific number of streams just because we can.
-define(SCTP_INIT, #sctp_initmsg{num_ostreams = 5,
                                 max_instreams = 5}).

%% Messages from gen_sctp.
-define(SCTP(Sock, Data), {sctp, Sock, _, _, Data}).

-define(A, list_to_atom).
-define(L, atom_to_list).

-define(TL(F),    ?TL(F, [])).
-define(TL(F, A), ?LOG("DTRANSPS", F, A)).


%% ===========================================================================
%% common_test wrapping

suite() ->
    [{timetrap, {seconds, 270}}].

all() ->
    [parallel].


init_per_suite(Config) ->
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?DUTIL:end_per_suite(Config).


parallel(_) ->
    ?TL("parallel -> entry"),
    Res = run(),
    ?TL("parallel -> done when"
        "~n   Res: ~p", [Res]),
    Res.


%% ===========================================================================

%% run/0

run() ->
    ok = diameter:start(),
    try
        ?RUN([[fun run/1, {P,F}]
              || P <- [sctp || ?HAVE_SCTP()] ++ [tcp],
                 F <- [connect, accept, reconnect, callback]])
    after
        diameter:stop()
    end.

%% run/1

run({Prot, reconnect}) ->
    ?TL("run(reconnect) -> entry with"
        "~n   Prot: ~p", [Prot]),
    Res = reconnect(Prot),
    ?TL("run(reconnect) -> done when"
        "~n   Res: ~p", [Res]),
    Res;

run({Prot, accept}) ->
    ?TL("run(accept) -> entry with"
        "~n   Prot: ~p", [Prot]),
    Res = accept(Prot),
    ?TL("run(accept) -> done when"
        "~n   Res: ~p", [Res]),
    Res;

run({Prot, connect}) ->
    ?TL("run(connect) -> entry with"
        "~n   Prot: ~p", [Prot]),
    Res = connect(Prot),
    ?TL("run(connect) -> done when"
        "~n   Res: ~p", [Res]),
    Res;

run({Prot, callback}) ->
    ?TL("run(callback) -> entry with"
        "~n   Prot: ~p", [Prot]),
    Res = callback(Prot),
    ?TL("run(callback) -> done when"
        "~n   Res: ~p", [Res]),
    Res.


%% ===========================================================================
%% accept/1
%%
%% diameter transport accepting, test code connecting.

accept(Prot) ->
    ?TL("accept -> entry with"
        "~n   Prot: ~p", [Prot]),

    Ref = make_ref(),
    true = diameter_reg:add_new({diameter_config, transport, Ref}), %% fake it
    T = {Prot, Ref},
    Res = ?RUN([{{?MODULE, [init, X, T]}, 15000}
                || X <- [accept, gen_connect]]),

    ?TL("accept -> done when"
        "~n   Res: ~p", [Res]),
    ok.


%% ===========================================================================
%% connect/1
%%
%% Test code accepting, diameter transport connecting.

connect(Prot) ->
    ?TL("connect -> entry with"
        "~n   Prot: ~p", [Prot]),

    T = {Prot, make_ref()},
    Res = ?RUN([{{?MODULE, [init, X, T]}, 15000}
                 || X <- [gen_accept, connect]]),

    ?TL("connect -> done when"
        "~n   Res: ~p", [Res]),
    ok.


%% ===========================================================================
%% reconnect/1
%%
%% Exercise reconnection behaviour: that a connecting transport
%% doesn't try to establish a new connection until the old one is
%% broken.

reconnect({listen, Ref}) ->
    ?TL("reconnect(listen) -> entry with"
        "~n   Ref: ~p", [Ref]),
    %5 SvcName = make_ref(),
    SvcName = {reconnect, listen, make_ref()},

    ?TL("reconnect(listen) -> register service"),
    ok = ?DEL_REG(SvcName),

    ?TL("reconnect(listen) -> start service (~p)", [SvcName]),
    ok = start_service(SvcName),
    ?TL("reconnect(listen) -> connect"),
    LRef = ?LISTEN(SvcName, tcp, [{watchdog_timer, 6000}]),
    ?TL("reconnect(listen) -> wait"),
    [_] = diameter_reg:wait({diameter_tcp, listener, {LRef, '_'}}),
    ?TL("reconnect(listen) -> register new transport"),
    true = diameter_reg:add_new({?MODULE, Ref, LRef}),

    %% Wait for partner to request transport death.
    ?TL("reconnect(listen) -> abort: await (request for) transport death"),
    TPid = abort(SvcName, LRef, Ref),

    %% Kill transport to force the peer to reconnect.
    ?TL("reconnect(listen) -> kill transport"),
    exit(TPid, kill),

    %% Wait for the partner again.
    ?TL("reconnect(listen) -> abort: wait for partner again"),
    Res = abort(SvcName, LRef, Ref),

    ?TL("reconnect(listen) -> unregister service"),
    ok = ?DEL_UNREG(SvcName),

    ?TL("reconnect(listen) -> done when"
        "~n   Res: ~p", [Res]),
    ok;

reconnect({connect, Ref}) ->
    ?TL("reconnect(connect) -> entry with"
        "~n   Ref: ~p", [Ref]),

    %% SvcName = make_ref(),
    SvcName = {reconnect, connect, make_ref()},

    ?TL("reconnect(connect) -> register service"),
    ok = ?DEL_REG(SvcName),

    ?TL("reconnect(connect) -> subscribe to service ~p", [SvcName]),
    true = diameter:subscribe(SvcName),
    ?TL("reconnect(connect) -> start service ~p", [SvcName]),
    ok = start_service(SvcName),
    ?TL("reconnect(connect) -> wait when"
        "~n   Svc transports:  ~p"
        "~n   Svc connections: ~p",
        [diameter:service_info(SvcName, transport),
         diameter:service_info(SvcName, connections)]),
    [{{_, _, LRef}, Pid}] = diameter_reg:wait({?MODULE, Ref, '_'}),
    ?TL("reconnect(connect) -> connect when"
        "~n   Svc transports:  ~p"
        "~n   Svc connections: ~p",
        [diameter:service_info(SvcName, transport),
         diameter:service_info(SvcName, connections)]),
    CRef = ?CONNECT(SvcName, tcp, LRef, [{connect_timer, 2000},
                                         {watchdog_timer, 6000}]),
    ?TL("reconnect(connect) -> connected when"
        "~n   Svc transports:  ~p"
        "~n   Svc connections: ~p",
        [diameter:service_info(SvcName, transport),
         diameter:service_info(SvcName, connections)]),

    %% Tell partner to kill transport after seeing that there are no
    %% reconnection attempts.
    ?TL("reconnect(connect) -> abort (kill transport)"),
    abort(SvcName, Pid, Ref),

    %% Transport goes down and is reestablished.
    ?TL("reconnect(connect) -> await transport down"),
    ?RECV(#diameter_event{service = SvcName, info = {down, CRef, _, _}}),
    ?TL("reconnect(connect) -> await transport reconnect"),
    ?RECV(#diameter_event{service = SvcName, info = {reconnect, CRef, _}}),
    ?TL("reconnect(connect) -> await transport up"),
    ?RECV(#diameter_event{service = SvcName, info = {up, CRef, _, _, _}}),

    %% Kill again.
    ?TL("reconnect(connect) -> abort (kill transport again)"),
    abort(SvcName, Pid, Ref),

    %% Wait for partner to die.
    ?TL("reconnect(connect) -> await partner death"),
    MRef = erlang:monitor(process, Pid),
    Res = ?RECV({'DOWN', MRef, process, _, _}),

    ?TL("reconnect(connect) -> unregister service"),
    ok = ?DEL_UNREG(SvcName),

    ?TL("reconnect(connect) -> done when"
        "~n   Res: ~p", [Res]),
    ok;

reconnect(Prot) ->
    ?TL("reconnect -> entry with"
        "~n   Prot: ~p", [Prot]),
    Ref = make_ref(),
    Res = ?RUN([{{?MODULE, [reconnect, {T, Ref}]}, 240000}
                || Prot == tcp,  %% ignore sctp
                   T <- [listen, connect]]),

    ?TL("reconnect -> done when"
        "~n   Res: ~p", [Res]),
    ok.

%% ===========================================================================
%% callback/1
%%
%% Check that when message callback is updated after message sending,
%% the ack is reported using the new callback.

callback(Prot) ->
    ?TL("callback -> entry with"
        "~n   Prot: ~p", [Prot]),

    ServerSvcName = {callback, listen, make_ref()},
    ?TL("callback -> register service ~p", [ServerSvcName]),
    ok = ?DEL_REG(ServerSvcName),
    ?TL("callback -> start service ~p", [ServerSvcName]),
    ok = start_service(ServerSvcName),
    ?TL("callback -> listen when"
        "~n   ServerSvcName: ~p", [ServerSvcName]),
    ServerProtOpts = [Prot, {message_cb, {?MODULE, message_server, [self(), 0]}}],
    LRef = ?LISTEN(ServerSvcName, ServerProtOpts, []),
    ?TL("callback -> wait"),
    [_] = diameter_reg:wait({?A("diameter_" ++ ?L(Prot)), listener, {LRef, '_'}}),

    ClientSvcName = {callback, connect, make_ref()},
    ?TL("callback -> register service ~p", [ClientSvcName]),
    ok = ?DEL_REG(ClientSvcName),
    ?TL("callback -> start service ~p", [ClientSvcName]),
    ok = start_service(ClientSvcName),
    ?TL("callback -> connect when"
        "~n   ClientSvcName: ~p"
        "~n   LRef:       ~p", [ClientSvcName, LRef]),
    ClientProtOpts = [Prot, {message_cb, {?MODULE, message_client, [self(), 0]}}],
    CRef = ?CONNECT(ClientSvcName, ClientProtOpts, LRef, [{connect_timer, 2000}]),
    ?TL("callback -> CRef: ~p", [CRef]),

    [{'Origin-Host', OH}, {'Origin-Realm', OR}] =
        diameter:service_info(ServerSvcName, ['Origin-Host', 'Origin-Realm']),
    Msg = ['ACR', {'Session-Id', "invalid"},
                  {'Origin-Host', OH},
                  {'Origin-Realm', OR},
                  {'Destination-Realm', OR},
                  {'Accounting-Record-Type', 1},
                  {'Accounting-Record-Number', 0}],

    diameter:call(ClientSvcName, diameter_gen_base_rfc6733, Msg, []),
    ok = verify_message_callbacks([]).

verify_message_callbacks(States) ->
    receive
        {Type, N} when Type == client; Type == server ->
            ?TL("verify_message_callbacks -> received"
                "~n   Type: ~p"
                "~n   N:    ~p", [Type, N]),
            case lists:member({Type, N}, States) of
                true ->
                    {error, "State already received previously"};
                false ->
                    verify_message_callbacks([{Type, N} | States])
            end
    after 1000 ->
        case length(States) of
            0 ->
                {error, "No message callbacks received"};
            _ ->
                ok
        end
    end.

message_client(ack, Msg, Parent, N) ->
    ?TL("message_client(ack) -> entry with"
        "~n   Dir: ack"
        "~n   Msg: ~p"
        "~n   N: ~p", [Msg, N]),
    Parent ! {client, N},
    [{?MODULE, ?FUNCTION_NAME, [Parent, N + 1]}];
message_client(Dir, Msg, Parent, N) ->
    ?TL("message_client -> entry with"
        "~n   Dir: ~p"
        "~n   Msg: ~p"
        "~n   N: ~p", [Dir, Msg, N]),
    Parent ! {client, N},
    [Msg, {?MODULE, ?FUNCTION_NAME, [Parent, N + 1]}].

message_server(ack, Msg, Parent, N) ->
    ?TL("message_server(ack) -> entry with"
        "~n   Dir: ack"
        "~n   Msg: ~p"
        "~n   N: ~p", [Msg, N]),
    Parent ! {server, N},
    [{?MODULE, ?FUNCTION_NAME, [Parent, N + 1]}];
message_server(Dir, Msg, Parent, N) ->
    ?TL("message_server -> entry with"
        "~n   Dir: ~p"
        "~n   Msg: ~p"
        "~n   N: ~p", [Dir, Msg, N]),
    Parent ! {server, N},
    [Msg, {?MODULE, ?FUNCTION_NAME, [Parent, N + 1]}].

start_service(SvcName) ->
    OH = diameter_util:unique_string(),
    Opts = [{application, [{dictionary, diameter_gen_base_rfc6733},
                           {module, diameter_callback}]},
            {'Origin-Host', OH},
            {'Origin-Realm', OH ++ ".org"},
            {'Vendor-Id', 0},
            {'Product-Name', "x"},
            {'Auth-Application-Id', [0]}],
    diameter:start_service(SvcName, Opts).

abort(SvcName, Pid, Ref)
  when is_pid(Pid) ->
    receive
        #diameter_event{service = SvcName, info = {reconnect, _, _}} = E ->
            erlang:error(E)
    after 45000 ->
            ok
    end,
    Pid ! {abort, Ref};

abort(SvcName, LRef, Ref)
  when is_reference(LRef) ->
    ?RECV({abort, Ref}),
    [[{ref, LRef}, {type, listen}, {options, _}, {accept, [_,_] = Ts} | _]]
                                                 %% assert on two accepting
        = diameter:service_info(SvcName, transport),
    [TPid] = [P || [{watchdog, {_,_,okay}}, {peer, {P,_}} | _] <- Ts],
    TPid.

%% ===========================================================================
%% ===========================================================================

%% init/2

init(accept, {Prot, Ref}) ->
    ?TL("init(accept) -> entry with"
        "~n   Prot: ~p"
        "~n   Ref:  ~p", [Prot, Ref]),

    %% Start an accepting transport and receive notification of a
    %% connection.
    TPid = start_accept(Prot, Ref),

    %% Receive a message and send it back.
    <<_:8, Len:24, _/binary>> = Bin = bin(Prot, ?RECV(?TMSG({recv, P}), P)),

    Len = size(Bin),
    TPid ! ?TMSG({send, Bin}),

    %% Expect the transport process to die as a result of the peer
    %% closing the connection.
    MRef = erlang:monitor(process, TPid),
    Res = ?RECV({'DOWN', MRef, process, _, _}),

    ?TL("init(accept) -> done when"
        "~n   Res: ~p", [Res]),
    ok;

init(gen_connect, {Prot, Ref}) ->
    ?TL("init(gen_connect) -> entry with"
        "~n   Prot: ~p"
        "~n   Ref:  ~p", [Prot, Ref]),

    %% Lookup the peer's listening socket.
    [PortNr] = ?LPORT(Prot, Ref),

    %% Connect, send a message and receive it back.
    {ok, Sock} = gen_connect(Prot, PortNr),
    Bin = make_msg(),
    ok = gen_send(Prot, Sock, Bin),
    Bin = gen_recv(Prot, Sock),

    ?TL("init(gen_connect) -> done"),
    ok;

init(gen_accept, {Prot, Ref}) ->
    ?TL("init(gen_accept) -> entry with"
        "~n   Prot: ~p"
        "~n   Ref:  ~p", [Prot, Ref]),

    %% Open a listening socket and publish the port number.
    {ok, LSock} = gen_listen(Prot),
    {ok, PortNr} = inet:port(LSock),
    true = diameter_reg:add_new(?TEST_LISTENER(Ref, PortNr)),

    %% Accept a connection, receive a message send it back, and wait
    %% for the peer to close the connection.
    {ok, Sock} = gen_accept(Prot, LSock),
    Bin = gen_recv(Prot, Sock),
    ok = gen_send(Prot, Sock, Bin),
    _Res = receive
               {tcp_closed, Sock} = T ->
                   T;
               ?SCTP(Sock, {_, #sctp_assoc_change{}}) = T ->
                   T
           end,

    ?TL("init(gen_accept) -> done when"
        "~n   Res: ~p", [_Res]),
    ok;

init(connect, {Prot, Ref}) ->
    ?TL("init(connect) -> entry with"
        "~n   Prot: ~p"
        "~n   Ref:  ~p", [Prot, Ref]),

    %% Lookup the peer's listening socket.
    [{?TEST_LISTENER(_, PortNr), _}]
        = diameter_reg:wait(?TEST_LISTENER(Ref, '_')),

    %% Start a connecting transport and receive notification of
    %% the connection.
    TPid = start_connect(Prot, PortNr, Ref),

    %% Send a message and receive it back.
    Bin = make_msg(),
    TPid ! ?TMSG({send, Bin}),
    Bin = bin(Prot, ?RECV(?TMSG({recv, P}), P)),

    ?TL("init(connect) -> done"),
    ok.

bin(sctp, #diameter_packet{bin = Bin}) ->
    Bin;
bin(tcp, Bin) ->
    Bin.

%% make_msg/0
%%
%% A valid Diameter message in as far as diameter_tcp examines it,
%% the module examining the length in the Diameter header to locate
%% message boundaries.

make_msg() ->
    N = 1024,
    Bin = rand_bytes(4*N),
    Len = 4*(N+1),
    <<1:8, Len:24, Bin/binary>>.

%% crypto:rand_bytes/1 isn't available on all platforms (since openssl
%% isn't) so roll our own. Not particularly random, but less verbose
%% in trace.
rand_bytes(N) ->
    Oct = rand:uniform(256) - 1,
    binary:copy(<<Oct>>, N).

%% ===========================================================================

%% start_connect/3

start_connect(Prot, PortNr, Ref) ->
    {ok, TPid} = start_connect(Prot,
                               {connect, Ref},
                               ?SVC([]),
                               [{raddr, ?ADDR},
                                {rport, PortNr},
                                {ip, ?ADDR},
                                {port, 0}]),
    connected(Prot, TPid),
    TPid.

connected(sctp, TPid) ->
    ?RECV(?TMSG({TPid, connected, _}));
connected(tcp, TPid) ->
    ?RECV(?TMSG({TPid, connected, _, [?ADDR]})).

start_connect(sctp, T, Svc, Opts) ->
    {ok, TPid, [?ADDR]}
        = diameter_sctp:start(T, Svc, [{sctp_initmsg, ?SCTP_INIT} | Opts]),
    {ok, TPid};
start_connect(tcp, T, Svc, Opts) ->
    diameter_tcp:start(T, Svc, Opts).

%% start_accept/2

start_accept(Prot, Ref) ->
    {ok, TPid, [?ADDR]}
        = start_accept(Prot, {accept, Ref}, ?SVC([?ADDR]), [{port, 0}]),
    ?RECV(?TMSG({TPid, connected})),
    TPid.

start_accept(sctp, T, Svc, Opts) ->
    diameter_sctp:start(T, Svc, [{sctp_initmsg, ?SCTP_INIT} | Opts]);
start_accept(tcp, T, Svc, Opts) ->
    diameter_tcp:start(T, Svc, Opts).

%% ===========================================================================

%% gen_connect/2

gen_connect(sctp = P, PortNr) ->
    {ok, Sock} = Ok = gen_sctp:open([{ip, ?ADDR}, {port, 0} | ?SCTP_OPTS]),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    Ok = gen_accept(P, Sock);
gen_connect(tcp, PortNr) ->
    gen_tcp:connect(?ADDR, PortNr, ?TCP_OPTS).

%% gen_listen/1

gen_listen(sctp) ->
    {ok, Sock} = gen_sctp:open([{ip, ?ADDR}, {port, 0} | ?SCTP_OPTS]),
    {gen_sctp:listen(Sock, true), Sock};
gen_listen(tcp) ->
    gen_tcp:listen(0, [{ip, ?ADDR} | ?TCP_OPTS]).

%% gen_accept/2

gen_accept(sctp, Sock) ->
    #sctp_assoc_change{state = comm_up,
                       outbound_streams = OS,
                       inbound_streams = IS,
                       assoc_id = Id}
        = ?RECV(?SCTP(Sock, {_, #sctp_assoc_change{} = S}), S),

    putr(assoc, {OS, IS, Id}),
    {ok, Sock};
gen_accept(tcp, LSock) ->
    gen_tcp:accept(LSock).

%% gen_send/3

gen_send(sctp, Sock, Bin) ->
    {OS, _IS, Id} = getr(assoc),
    case gen_sctp:send(Sock, Id, erlang:unique_integer([positive]) rem OS, Bin) of
        ok ->
            ok;
        {error, Reason} = ERROR ->
            Info = try inet:info(Sock) of
                       I ->
                           I
                   catch
                       C:E:S ->
                           [{class,  C},
                            {error,  E},
                            {stack,  S}]
                   end,
            ?TL("Failed (sctp) sending message: "
                "~n   Reason:        ~p"
                "~n   Socket:        ~p"
                "~n   (Socket) Info: ~p", [Reason, Sock, Info]),
            ERROR
    end;
gen_send(tcp, Sock, Bin) ->
    case gen_tcp:send(Sock, Bin) of
        ok ->
            ok;
        {error, Reason} = ERROR ->
            Info = try inet:info(Sock) of
                       I ->
                           I
                   catch
                       C:E:S ->
                           [{class,  C},
                            {error,  E},
                            {stack,  S}]
                   end,
            ?TL("Failed (tcp) sending message: "
                "~n   Reason:        ~p"
                "~n   Socket:        ~p"
                "~n   (Socket) Info: ~p", [Reason, Sock, Info]),
            ERROR
    end.

%% gen_recv/2

gen_recv(sctp, Sock) ->
    {OS, IS, Id} = getr(assoc),
    receive
        ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = I} = INFO], Bin})
          when is_binary(Bin) ->
            case {I, Id} of
                {Id, _} -> % assert
                    Bin;
                _ ->
                    ?TL("unexpected assoc id in received info msg:"
                        "~n   Expected Assoc ID: ~p"
                        "~n      OS: ~p"
                        "~n      IS: ~p"
                        "~n   Received Assoc ID: ~p"
                        "~n   Info:              ~p", [Id, OS, IS, I, INFO]),
                    ct:fail({unexpected_assoc_id, I, Id})
            end
    end;
gen_recv(tcp, Sock) ->
    tcp_recv(Sock, <<>>).

tcp_recv(_, <<_:8, Len:24, _/binary>> = Bin)
  when Len =< size(Bin) ->
    Bin;
tcp_recv(Sock, B) ->
    receive {tcp, Sock, Bin} -> tcp_recv(Sock, <<B/binary, Bin/binary>>) end.

%% putr/2

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

%% getr/1

getr(Key) ->
    get({?MODULE, Key}).
