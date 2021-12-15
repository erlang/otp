%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2021. All Rights Reserved.
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
%% Tests of traffic between two Diameter nodes, the client being
%% spread across three Erlang nodes.
%%

-module(diameter_distribution_SUITE).

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% testcases
-export([send_local/1,
         send_remote/1,
         send_timeout/1,
         send_failover/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

%% Internal export for RPC
-export([start_server/5, start_client/6, call/1]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").

-include_lib("common_test/include/ct.hrl").

%% ===========================================================================

-define(CLIENT, 'CLIENT').
-define(SERVER, 'SERVER').
-define(REALM, "erlang.org").
-define(DICT, diameter_gen_base_rfc6733).
-define(ADDR, {127,0,0,1}).
-define(SUCCESS, 2001).
-define(BUSY,    3004).
-define(LOGOUT,  ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(MOVED,   ?'DIAMETER_BASE_TERMINATION-CAUSE_USER_MOVED').
-define(TIMEOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_SESSION_TIMEOUT').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [send_local,
     send_remote,
     send_timeout,
     send_failover].

init_per_suite(Config) ->
    {ok, ServerPeer, ServerNode} = ?CT_PEER(),
    ListenPort = erpc:call(ServerNode, ?MODULE, start_server, [?SERVER, "server0", 99, 0, 32]),
    %% client nodes
    Indices = lists:seq(0, 2),
    {_Ok, ClientPeers, Nodes} = lists:unzip3([{ok, _P, _N} = ?CT_PEER() || _ <- Indices]),
    %% Ensure the client nodes are connected since the sharing of
    %% transports is only between connected nodes.
    [pong = erpc:call(N, net_adm, ping, [M]) || N <- Nodes, M <- Nodes, N < M],
    %% Start diameter clients
    %% not exactly multi-call, call args are different
    [ok = erpc:call(N, ?MODULE, start_client,
        [?CLIENT, ListenPort, "client" ++ integer_to_list(Seq), Seq, Seq, 30])
        || {N, Seq} <- lists:zip(Nodes, Indices)],
    %% unlink peers, assuming end_per_suite is executed reliably
    Peers = [ServerPeer | ClientPeers],
    [unlink(P) || P <- Peers],
    [{server, ServerNode}, {clients, Nodes}, {peers, Peers} | Config].

end_per_suite(Config) ->
    [peer:stop(P) || P <- proplists:get_value(peers, Config)].

%% ===========================================================================
%% traffic testcases

%% send_local/1
%%
%% Send a request from the first client node, using a the local
%% transport.

send_local(Config) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Config, local, str(?LOGOUT)).

%% send_remote/1
%%
%% Send a request from the first client node, using a transport on the
%% another node.

send_remote(Config) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Config, remote, str(?LOGOUT)).

%% send_timeout/1
%%
%% Send a request that the server discards.

send_timeout(Config) ->
    {error, timeout} = send(Config, remote, str(?TIMEOUT)).

%% send_failover/1
%%
%% Send a request that causes the server to remote transports down.

send_failover(Config) ->
    #'diameter_base_answer-message'{'Result-Code' = ?BUSY}
        = send(Config, remote, str(?MOVED)).

%% ===========================================================================

str(Cause) ->
    #diameter_base_STR{'Destination-Realm'   = ?REALM,
                       'Auth-Application-Id' = ?DICT:id(),
                       'Termination-Cause'   = Cause}.

%% send/2

send(Config, Where, Req) ->
    [Node | _] = proplists:get_value(clients, Config) ,
    rpc:call(Node, ?MODULE, call, [{Where, Req}]).

%% call/1

call({Where, Req}) ->
    diameter:call(?CLIENT, ?DICT, Req, [{extra, [{Where, node()}]}]).


%% Starts a peer node with Diameter server running
start_server(Server, Host, Origin, SeqFrom, SeqTo) ->
    ok = diameter:start(),
    ok = diameter:start_service(Server, service_opt(Host, Origin, SeqFrom, SeqTo)),
    %% Listener config
    ListenConfig = [{transport_module, diameter_tcp}, {transport_config, [{ip, ?ADDR},
        {port, 0}, {accept, {256, 0, 0, 1}}, {accept, ["256.0.0.1", ["^.+$"]]}]}],
    %% start listener on Server0, get the listener port number
    {ok, ListenRef} = diameter:add_transport(Server, {listen, ListenConfig}),
    %% wait for listener to be up
    [_] = diameter_reg:wait({diameter_tcp, listener, {ListenRef, '_'}}),
    [{listen, ListenPort, _}] = diameter_tcp:ports(),
    ListenPort.

%% Starts Diameter client node connected to Server
start_client(Client, ServerPort, Host, Origin, SeqFrom, SeqTo) ->
    ok = diameter:start(),
    ok = diameter:start_service(Client, service_opt(Host, Origin, SeqFrom, SeqTo)),
    %% Establish connection to the server
    ConnectConfig = [{transport_module, diameter_tcp},
        {transport_config, [{ip, ?ADDR}, {port, 0}, {raddr, ?ADDR}, {rport, ServerPort}]}],
    true = diameter:subscribe(Client),
    {ok, Ref} = diameter:add_transport(Client, {connect, ConnectConfig}),
    receive
        {diameter_event, Client, {up, Ref, _, _, _}} -> ok
    end.

%% Config for diameter:start_service/2.
service_opt(Host, Origin, SeqFrom, SeqTo) ->
    [{'Origin-Host', Host ++ [$.|?REALM]},
        {'Origin-Realm', ?REALM},
        {'Host-IP-Address', [?ADDR]},
        {'Vendor-Id', 12345},
        {'Product-Name', "OTP/diameter"},
        {'Auth-Application-Id', [?DICT:id()]},
        {'Origin-State-Id', Origin},
        {share_peers, peers(Host)},
        {use_shared_peers, peers(Host)},
        {restrict_connections, false},
        {spawn_opt, {diameter_dist, spawn_local, []}},
        {sequence, fun () -> {SeqFrom, SeqTo} end},
        {application, [{dictionary, ?DICT},
            {module, ?MODULE},
            {request_errors, callback},
            {answer_errors, callback}]}].

peers("server0")  -> true;
peers("client0") -> [node() | nodes()];
peers("client1") -> fun erlang:nodes/0;
peers("client2") -> nodes().

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([LP], [_, _], ?CLIENT, _State, {local, _Client0Node}) ->
    {ok, LP};

pick_peer([_], [RP | _], ?CLIENT, _State, {remote, _Client0Node}) ->
    {ok, RP};

pick_peer([LP], [], ?CLIENT, _State, {remote, _Client0Node}) ->
    {ok, LP}.

%% prepare_request/4

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, {_, _Client0Node}) ->
    #diameter_packet{msg = Req}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {send, Req#diameter_base_STR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Session-Id' = diameter:session_id(OH)}}.

%% prepare_retransmit/4

prepare_retransmit(Pkt, ?CLIENT, _, {_, _Client0Node}) ->
    #diameter_packet{msg = #diameter_base_STR{'Termination-Cause' = ?MOVED}}
        = Pkt,  %% assert
    {send, Pkt}.

%% handle_answer/5

handle_answer(Pkt, _Req, ?CLIENT, _Peer, {_, _Client0Node}) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer, {_, _Client0Node}) ->
    {error, Reason}.

%% handle_request/3

handle_request(Pkt, ?SERVER, Peer) ->
    #diameter_packet{msg = Req}
        = Pkt,
    request(Req, Peer).

request(#diameter_base_STR{'Termination-Cause' = ?TIMEOUT}, _) ->
    discard;

request(#diameter_base_STR{'Termination-Cause' = ?MOVED}, Peer) ->
    {TPid, #diameter_caps{origin_state_id = {_, [N]}}} = Peer,
    fail(N, TPid);

request(#diameter_base_STR{'Session-Id' = SId}, {_, Caps}) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}}.

fail(0, _) ->     %% sent from the originating node ...
    {protocol_error, ?BUSY};

fail(_, TPid) ->  %% ... or through a remote node: force failover
    exit(TPid, kill),
    discard.
