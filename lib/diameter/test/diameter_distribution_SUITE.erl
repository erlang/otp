%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2022. All Rights Reserved.
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

%% testcases, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([suite/0,
         all/0,
         traffic/1]).

%% rpc calls
-export([ping/1,
         start/1,
         connect/1,
         call/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(CLIENT, 'CLIENT').
-define(SERVER, 'SERVER').
-define(REALM, "erlang.org").
-define(DICT, diameter_gen_base_rfc6733).
-define(ADDR, {127,0,0,1}).

%% Config for diameter:start_service/2.
-define(SERVICE(Host),
        [{'Origin-Host', Host ++ [$.|?REALM]},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DICT:id()]},
         {'Origin-State-Id', origin()},
         {share_peers, peers()},
         {use_shared_peers, peers()},
         {restrict_connections, false},
         {spawn_opt, {diameter_dist, spawn_local, []}},
         {sequence, fun sequence/0},
         {application, [{dictionary, ?DICT},
                        {module, ?MODULE},
                        {request_errors, callback},
                        {answer_errors, callback}]}]).

-define(SUCCESS, 2001).
-define(BUSY,    3004).
-define(LOGOUT,  ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(MOVED,   ?'DIAMETER_BASE_TERMINATION-CAUSE_USER_MOVED').
-define(TIMEOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_SESSION_TIMEOUT').

-define(L, atom_to_list).
-define(A, list_to_atom).

%% The order here is significant and causes the server to listen
%% before the clients connect.
-define(NODES, [{server, ?SERVER},
                {client0, ?CLIENT},
                {client1, ?CLIENT},
                {client2, ?CLIENT}]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [traffic].

traffic(_Config) ->
    traffic().

%% ===========================================================================

run() ->
    [] = ?util:run([{fun traffic/0, 60000}]).
    %% process for linked peers to die with

%% traffic/0

traffic() ->
    true = is_alive(),  %% need distribution for peer nodes
    Nodes = enslave(),
    [] = ping(Nodes),  %% drop client node
    [] = start(Nodes),
    [_] = connect(Nodes),
    [] = send(Nodes).

%% enslave/0
%%
%% Start four slave nodes, one to implement a Diameter server,
%% three to implement a client.

enslave() ->
    Here = filename:dirname(code:which(?MODULE)),
    Ebin = filename:join([Here, "..", "ebin"]),
    Args = ["-pa", Here, Ebin],
    [{N,S} || {M,S} <- ?NODES, N <- [start(M, Args)]].

start(Name, Args) ->
    {ok, _, Node} = ?util:peer(#{name => Name, args => Args}),
    Node.

%% ping/1
%%
%% Ensure the client nodes are connected since the sharing of
%% transports is only between connected nodes.

ping({?SERVER, _Nodes}) ->
    [];

ping({?CLIENT, Nodes}) ->
    [N || {N,_} <- Nodes,
          node() /= N,
          pang <- [net_adm:ping(N)]];

ping(Nodes) ->
    [{N,RC} || {N,S} <- Nodes,
               RC <- [rpc:call(N, ?MODULE, ping, [{S, Nodes}])],
               RC /= []].

%% start/1
%%
%% Start diameter services.

start(SvcName)
  when is_atom(SvcName) ->
    ok = diameter:start(),
    ok = diameter:start_service(SvcName, ?SERVICE((?L(SvcName))));

start(Nodes) ->
    [{N,RC} || {N,S} <- Nodes,
               RC <- [rpc:call(N, ?MODULE, start, [S])],
               RC /= ok].

sequence() ->
    sequence(sname()).

sequence(server) ->
    {0,32};
sequence(Client) ->
    "client" ++ N = ?L(Client),
    {list_to_integer(N), 30}.

origin() ->
    origin(sname()).

origin(server) ->
    99;
origin(Client) ->
    "client" ++ N = ?L(Client),
    list_to_integer(N).

peers() ->
    peers(sname()).

peers(server)  -> true;
peers(client0) -> [node() | nodes()];
peers(client1) -> fun erlang:nodes/0;
peers(client2) -> nodes().

%% connect/1
%%
%% Establish one connection to the server from each of the client
%% nodes.

connect({?SERVER, _, []}) ->
    [_LRef = ?util:listen(?SERVER, tcp)];

connect({?CLIENT, [{Node, _} | _], [LRef] = Acc}) ->
    ?util:connect(?CLIENT, tcp, {Node, LRef}),
    Acc;

connect(Nodes) ->
    lists:foldl(fun({N,S}, A) ->
                        rpc:call(N, ?MODULE, connect, [{S, Nodes, A}])
                end,
                [],
                Nodes).

%% ===========================================================================

%% send/1

send(Nodes) ->
    ?util:run([[fun send/2, Nodes, T]
               || T <- [local, remote, timeout, failover]]).

%% send/2

%% Send a request from the first client node, using a the local
%% transport.
send(Nodes, local) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Nodes, 0, str(?LOGOUT));

%% Send a request from the first client node, using a transport on the
%% another node.
send(Nodes, remote) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Nodes, 1, str(?LOGOUT));

%% Send a request that the server discards.
send(Nodes, timeout) ->
    {error, timeout} = send(Nodes, 1, str(?TIMEOUT));

%% Send a request that causes the server to take the transport down.
send(Nodes, failover) ->
    #'diameter_base_answer-message'{'Result-Code' = ?BUSY}
        = send(Nodes, 2, str(?MOVED)).

%% ===========================================================================

str(Cause) ->
    #diameter_base_STR{'Destination-Realm'   = ?REALM,
                       'Auth-Application-Id' = ?DICT:id(),
                       'Termination-Cause'   = Cause}.

%% send/3

send([_, {Node, _} | _], Where, Req) ->
    rpc:call(Node, ?MODULE, call, [{Where, Req}]).

%% call/1

call({Where, Req}) ->
    diameter:call(?CLIENT, ?DICT, Req, [{extra, [{Where, sname()}]}]).

%% sname/0

sname() ->
    ?A(hd(string:tokens(?L(node()), "@"))).

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([LP], _, ?CLIENT, _State, {0, client0}) ->
    {ok, LP};

pick_peer(_, Peers, ?CLIENT, _State, {1, client0}) ->
    [RP] = [T || {_, #diameter_caps{origin_state_id = {[1],_}}} = T <- Peers],
    {ok, RP};

%% Sending on the remote transport causes the server to take the
%% transport down. Retransmission on the local.
pick_peer(LP, RP, ?CLIENT, _State, {2, client0}) ->
    {ok, case [T || {_, #diameter_caps{origin_state_id = {[2],_}}} = T <- RP] of
             [T] ->
                 T;
             _ ->
                 hd([_] = LP)
         end}.

%% prepare_request/4

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, {_, client0}) ->
    #diameter_packet{msg = Req}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {send, Req#diameter_base_STR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Session-Id' = diameter:session_id(OH)}}.

%% prepare_retransmit/4

prepare_retransmit(Pkt, ?CLIENT, _, {_, client0}) ->
    #diameter_packet{msg = #diameter_base_STR{'Termination-Cause' = ?MOVED}}
        = Pkt,  %% assert
    {send, Pkt}.

%% handle_answer/5

handle_answer(Pkt, _Req, ?CLIENT, _Peer, {_, client0}) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer, {_, client0}) ->
    {error, Reason}.

%% handle_request/3

handle_request(Pkt, ?SERVER, Peer) ->
    server = sname(),  %% assert
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
