%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2022. All Rights Reserved.
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
-export([suite/0,
         all/0,
         parallel/1]).

-export([accept/1,
         connect/1,
         reconnect/1,
         init/2]).

-include_lib("kernel/include/inet_sctp.hrl").
-include("diameter.hrl").

-define(util, diameter_util).

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

%% ===========================================================================
%% common_test wrapping

suite() ->
    [{timetrap, {seconds, 270}}].

all() ->
    [parallel].

parallel(_) ->
    run().

%% ===========================================================================

%% run/0

run() ->
    ok = diameter:start(),
    try
        ?util:run([[fun run/1, {P,F}]
                   || P <- [sctp || ?util:have_sctp()] ++ [tcp],
                      F <- [connect, accept, reconnect]])
    after
        diameter:stop()
    end.

%% run/1

run({Prot, reconnect}) ->
    reconnect(Prot);

run({Prot, accept}) ->
    accept(Prot);

run({Prot, connect}) ->
    connect(Prot).

%% ===========================================================================
%% accept/1
%%
%% diameter transport accepting, test code connecting.

accept(Prot) ->
    Ref = make_ref(),
    true = diameter_reg:add_new({diameter_config, transport, Ref}), %% fake it
    T = {Prot, Ref},
    ?util:run([{{?MODULE, [init, X, T]}, 15000}
               || X <- [accept, gen_connect]]).

%% ===========================================================================
%% connect/1
%%
%% Test code accepting, diameter transport connecting.

connect(Prot) ->
    T = {Prot, make_ref()},
    ?util:run([{{?MODULE, [init, X, T]}, 15000}
               || X <- [gen_accept, connect]]).

%% ===========================================================================
%% reconnect/1
%%
%% Exercise reconnection behaviour: that a connecting transport
%% doesn't try to establish a new connection until the old one is
%% broken.

reconnect({listen, Ref}) ->
    SvcName = make_ref(),
    ok = start_service(SvcName),
    LRef = ?util:listen(SvcName, tcp, [{watchdog_timer, 6000}]),
    [_] = diameter_reg:wait({diameter_tcp, listener, {LRef, '_'}}),
    true = diameter_reg:add_new({?MODULE, Ref, LRef}),

    %% Wait for partner to request transport death.
    TPid = abort(SvcName, LRef, Ref),

    %% Kill transport to force the peer to reconnect.
    exit(TPid, kill),

    %% Wait for the partner again.
    abort(SvcName, LRef, Ref);

reconnect({connect, Ref}) ->
    SvcName = make_ref(),
    true = diameter:subscribe(SvcName),
    ok = start_service(SvcName),
    [{{_, _, LRef}, Pid}] = diameter_reg:wait({?MODULE, Ref, '_'}),
    CRef = ?util:connect(SvcName, tcp, LRef, [{connect_timer, 2000},
                                              {watchdog_timer, 6000}]),

    %% Tell partner to kill transport after seeing that there are no
    %% reconnection attempts.
    abort(SvcName, Pid, Ref),

    %% Transport goes down and is reestablished.
    ?RECV(#diameter_event{service = SvcName, info = {down, CRef, _, _}}),
    ?RECV(#diameter_event{service = SvcName, info = {reconnect, CRef, _}}),
    ?RECV(#diameter_event{service = SvcName, info = {up, CRef, _, _, _}}),

    %% Kill again.
    abort(SvcName, Pid, Ref),

    %% Wait for partner to die.
    MRef = erlang:monitor(process, Pid),
    ?RECV({'DOWN', MRef, process, _, _});

reconnect(Prot) ->
    Ref = make_ref(),
    ?util:run([{{?MODULE, [reconnect, {T, Ref}]}, 240000}
               || Prot == tcp,  %% ignore sctp
                  T <- [listen, connect]]).

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
    ?RECV({'DOWN', MRef, process, _, _});

init(gen_connect, {Prot, Ref}) ->
    %% Lookup the peer's listening socket.
    [PortNr] = ?util:lport(Prot, Ref),

    %% Connect, send a message and receive it back.
    {ok, Sock} = gen_connect(Prot, PortNr),
    Bin = make_msg(),
    ok = gen_send(Prot, Sock, Bin),
    Bin = gen_recv(Prot, Sock);

init(gen_accept, {Prot, Ref}) ->
    %% Open a listening socket and publish the port number.
    {ok, LSock} = gen_listen(Prot),
    {ok, PortNr} = inet:port(LSock),
    true = diameter_reg:add_new(?TEST_LISTENER(Ref, PortNr)),

    %% Accept a connection, receive a message send it back, and wait
    %% for the peer to close the connection.
    {ok, Sock} = gen_accept(Prot, LSock),
    Bin = gen_recv(Prot, Sock),
    ok = gen_send(Prot, Sock, Bin),
    receive
        {tcp_closed, Sock} = T ->
            T;
        ?SCTP(Sock, {_, #sctp_assoc_change{}}) = T ->
            T
    end;

init(connect, {Prot, Ref}) ->
    %% Lookup the peer's listening socket.
    [{?TEST_LISTENER(_, PortNr), _}]
        = diameter_reg:wait(?TEST_LISTENER(Ref, '_')),

    %% Start a connecting transport and receive notification of
    %% the connection.
    TPid = start_connect(Prot, PortNr, Ref),

    %% Send a message and receive it back.
    Bin = make_msg(),
    TPid ! ?TMSG({send, Bin}),
    Bin = bin(Prot, ?RECV(?TMSG({recv, P}), P)).

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
    gen_sctp:send(Sock, Id, erlang:unique_integer([positive]) rem OS, Bin);
gen_send(tcp, Sock, Bin) ->
    gen_tcp:send(Sock, Bin).

%% gen_recv/2

gen_recv(sctp, Sock) ->
    {_OS, _IS, Id} = getr(assoc),
    receive
        ?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = I}], Bin})
          when is_binary(Bin) ->
            {Id, _} = {I, Id},  %% assert
            Bin
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
