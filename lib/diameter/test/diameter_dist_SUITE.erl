%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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
%% Tests of traffic between two Diameter nodes, the server being
%% spread across three Erlang nodes.
%%

-module(diameter_dist_SUITE).

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% testcases
-export([send/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

%% Internal export for RPC
-export([start_server/5, start_client/6]).

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
    [send].

init_per_suite(Config) ->
    {ok, ServerPeer, ServerNode} = ?CT_PEER(),
    ListenPort = erpc:call(ServerNode, ?MODULE, start_server, [?SERVER, "server0", 0, 0, 30]),
    %% Distributed workers
    Peer1 = start_worker(?SERVER),
    Peer2 = start_worker(?SERVER),
    %% Diameter client
    {ok, ClientPeer, ClientNode} = ?CT_PEER(),
    ok = erpc:call(ClientNode, ?MODULE, start_client, [?CLIENT, ListenPort, "client", 99, 0, 32]),
    %% unlink peers, assuming end_per_suite is executed reliably
    Peers = [ServerPeer, Peer1, Peer2, ClientPeer],
    [unlink(P) || P <- Peers],
    [{server, ServerNode}, {client, ClientNode}, {peers, Peers} | Config].

end_per_suite(Config) ->
    [peer:stop(P) || P <- proplists:get_value(peers, Config)].

%% Basic test
send(Config) when is_list(Config) ->
    ServerNode = proplists:get_value(server, Config),
    ClientNode = proplists:get_value(client, Config),
    %% Send 100 requests and ensure the node name sent as User-Name isn't
    %% the node terminating transport.
    send(ServerNode, ClientNode, 100, dict:new()).

send(Server0, _Client, 0, Dict) ->
    Node = atom_to_binary(Server0, utf8),
    {false, _} = {dict:is_key(Node, Dict), dict:to_list(Dict)},
    %% Check that counters have been incremented as expected on server0.
    [Info] = rpc:call(Server0, diameter, service_info, [?SERVER, connections]),
    {[Stats], _} = {[S || {statistics, S} <- Info], Info},
    {[{recv, 1, 100}, {send, 0, 100}], _}
        = {[{D,R,N} || T <- [recv, send],
                       {{{0,275,R}, D}, N} <- Stats,
                       D == T],
           Stats},
    {[{send, 0, 100, 2001}], _}
        = {[{D,R,N,C} || {{{0,275,R}, D, {'Result-Code', C}}, N} <- Stats],
           Stats};

send(Server0, Client, N, Dict) ->
    Req = #diameter_base_STR{'Destination-Realm'   = ?REALM,
        'Auth-Application-Id' = ?DICT:id(),
        'Termination-Cause'   = ?LOGOUT},
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'User-Name' = [ServerNode]}
        = rpc:call(Client, diameter, call, [?CLIENT, ?DICT, Req, []]),
    true = is_binary(ServerNode),
    send(Server0, Client, N-1, dict:update_counter(ServerNode, 1, Dict)).

%% ===========================================================================
%% Utilities

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
        {transport_config, [{raddr, ?ADDR}, {rport, ServerPort}]}],
    true = diameter:subscribe(Client),
    {ok, Ref} = diameter:add_transport(Client, {connect, ConnectConfig}),
    receive
        {diameter_event, Client, {up, Ref, _, _, _}} -> ok
    end.

%% Returns config for diameter:start_service/2.
service_opt(Host, Origin, SeqFrom, SeqTo) ->
    [{'Origin-Host', Host ++ [$.|?REALM]},
        {'Origin-Realm', ?REALM},
        {'Host-IP-Address', [?ADDR]},
        {'Vendor-Id', 12345},
        {'Product-Name', "OTP/diameter"},
        {'Auth-Application-Id', [?DICT:id()]},
        {'Origin-State-Id', Origin},
        {spawn_opt, {diameter_dist, route_session, [#{id => []}]}},
        {sequence, fun () -> {SeqFrom, SeqTo} end}, %% use 'fun' for fun and piggy-back testing
        {string_decode, false},
        {application, [{dictionary, ?DICT},
            {module, ?MODULE},
            {request_errors, callback},
            {answer_errors, callback}]}].

%% Starts a distributed worker node
start_worker(AttachTo) ->
    {ok, Peer, Node} = ?CT_PEER(),
    {ok, _} = erpc:call(Node, gen_server, start, [{local, diameter_dist}, diameter_dist, [], []]),
    ok = erpc:call(Node, diameter_dist, attach, [[AttachTo]]),
    Peer.

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([Peer], [], ?CLIENT, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}) ->
    #diameter_packet{msg = Req}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {send, Req#diameter_base_STR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Session-Id' = diameter:session_id(OH)}}.

%% prepare_retransmit/3

prepare_retransmit(_, ?CLIENT, _) ->
    discard.

%% handle_answer/5

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(Pkt, ?SERVER, {_, Caps}) ->
    #diameter_packet{msg = #diameter_base_STR{'Session-Id' = SId}}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'User-Name' = [atom_to_binary(node(), utf8)]}}.
