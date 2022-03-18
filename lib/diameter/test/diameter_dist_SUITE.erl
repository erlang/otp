%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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

%% all tests, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([suite/0,
         all/0,
         traffic/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

%% rpc calls
-export([start/1,
         call/1,
         connect/1,
         ping/1]).

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
         {spawn_opt, {diameter_dist, route_session, [#{id => []}]}},
         {sequence, fun sequence/0},
         {string_decode, false},
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
%% before the clients connect. The server listens on the first node,
%% and distributes requests to the other two.
-define(NODES, [{server0, ?SERVER},
                {server1, ?SERVER},
                {server2, ?SERVER},
                {client, ?CLIENT}]).

%% ===========================================================================
%% common_test wrapping

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [traffic].

traffic(_Config) ->
    run().

%% ===========================================================================

%% run/0

run() ->
    [] = ?util:run([{fun traffic/0, 60000}]).
    %% process for linked peers to die with

%% traffic/0

traffic() ->
    true = is_alive(),  %% need distribution for peer nodes
    Nodes = enslave(),
    [] = ping(lists:droplast(Nodes)),  %% drop client node
    [] = start(Nodes),
    ok = connect(Nodes),
    ok = send(Nodes).

%% enslave/1
%%
%% Start four slave nodes, three to implement a Diameter server,
%% one to implement a client.

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
%% Ensure the server nodes are connected so that diameter_dist can attach.

ping({S, Nodes}) ->
    ?SERVER = S,  %% assert
    [N || {N,_} <- Nodes,
          node() /= N,
          pang <- [net_adm:ping(N)]];

ping(Nodes) ->
    [{N,RC} || {N,S} <- Nodes,
               RC <- [rpc:call(N, ?MODULE, ping, [{S,Nodes}])],
               RC /= []].

%% start/1
%%
%% Start diameter services.

%% There's no need to start diameter on a node that only services
%% diameter_dist as a handler of incoming requests, but the
%% diameter_dist server must be started since the servers communicate
%% to determine who services what. The typical case is probably that
%% handler nodes also want to be able to send Diameter requests, in
%% which case the application needs to be started and diameter_dist is
%% started as a part of this, but only start the server here to ensure
%% everything still works as expected.
start({_SvcName, [_, {S1, _}, {S2, _}, _]})
  when node() == S1;    %% server1
       node() == S2 ->  %% server2
    Mod = diameter_dist,
    {ok, _} = gen_server:start({local, Mod}, Mod, _Args = [], _Opts  = []),
    ok;

start({SvcName, [{S0, _}, _, _, {C, _}]})
  when node() == S0;    %% server0
       node() == C ->   %% client
    ok = diameter:start(),
    ok = diameter:start_service(SvcName, ?SERVICE((?L(SvcName))));

start(Nodes) ->
    [{N,RC} || {N,S} <- Nodes,
               RC <- [rpc:call(N, ?MODULE, start, [{S, Nodes}])],
               RC /= ok].

sequence() ->
    sequence(sname()).

sequence(client) ->
    {0,32};
sequence(Server) ->
    "server" ++ N = ?L(Server),
    {list_to_integer(N), 30}.

origin() ->
    origin(sname()).

origin(client) ->
    99;
origin(Server) ->
    "server" ++ N = ?L(Server),
    list_to_integer(N).

%% connect/1
%%
%% Establish one connection from the client, terminated on the first
%% server node, the others handling requests.

connect({?SERVER, [{Node, _} | _], []})
  when Node == node() ->  %% server0
    [_LRef = ?util:listen(?SERVER, tcp)];

connect({?SERVER, _, [_] = Acc}) -> %% server[12]: register to receive requests
    ok = diameter_dist:attach([?SERVER]),
    Acc;

connect({?CLIENT, [{Node, _} | _], [LRef]}) ->
    ?util:connect(?CLIENT, tcp, {Node, LRef}),
    ok;

connect(Nodes) ->
    lists:foldl(fun({N,S}, A) ->
                        rpc:call(N, ?MODULE, connect, [{S, Nodes, A}])
                end,
                [],
                Nodes).

%% ===========================================================================
%% traffic testcases

%% send/1
%%
%% Send 100 requests and ensure the node name sent as User-Name isn't
%% the node terminating transport.

send(Nodes) ->
    send(Nodes, 100, dict:new()).

%% send/2

send(Nodes, 0, Dict) ->
    [{Server0, _} | _] = Nodes,
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
           Stats},
    ok;

send(Nodes, N, Dict) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'User-Name' = [ServerNode]}
        = send(Nodes, str(?LOGOUT)),
    true = is_binary(ServerNode),
    send(Nodes, N-1, dict:update_counter(ServerNode, 1, Dict)).

%% ===========================================================================

str(Cause) ->
    #diameter_base_STR{'Destination-Realm'   = ?REALM,
                       'Auth-Application-Id' = ?DICT:id(),
                       'Termination-Cause'   = Cause}.

%% send/2

send(Nodes, Req) ->
    {Node, _} = lists:last(Nodes),
    rpc:call(Node, ?MODULE, call, [Req]).

%% call/1

call(Req) ->
    diameter:call(?CLIENT, ?DICT, Req, []).

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
