%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-export([suite/0,
         all/0]).

%% testcases
-export([enslave/1, enslave/0,
         ping/1,
         start/1,
         connect/1,
         send/1,
         stop/1, stop/0]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-export([call/1]).

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

%% Options to ct_slave:start/2.
-define(TIMEOUTS, [{T, 15000} || T <- [boot_timeout,
                                       init_timeout,
                                       start_timeout]]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [enslave,
     ping,
     start,
     connect,
     send,
     stop].

%% ===========================================================================
%% start/stop testcases

%% enslave/1
%%
%% Start four slave nodes, three to implement a Diameter server,
%% one to implement a client.

enslave() ->
    [{timetrap, {seconds, 30*length(?NODES)}}].

enslave(Config) ->
    Here = filename:dirname(code:which(?MODULE)),
    Ebin = filename:join([Here, "..", "ebin"]),
    Dirs = [Here, Ebin],
    Nodes = [{N,S} || {M,S} <- ?NODES, N <- [slave(M, Dirs)]],
    ?util:write_priv(Config, nodes, [{N,S} || {{N,ok},S} <- Nodes]),
    [] = [{T,S} || {{_,E} = T, S} <- Nodes, E /= ok].

slave(Name, Dirs) ->
    add_pathsa(Dirs, ct_slave:start(Name, ?TIMEOUTS)).

add_pathsa(Dirs, {ok, Node}) ->
    {Node, rpc:call(Node, code, add_pathsa, [Dirs])};
add_pathsa(_, No) ->
    {No, error}.

%% ping/1
%%
%% Ensure the server nodes are connected so that diameter_dist can attach.

ping({S, Nodes}) ->
    ?SERVER = S,
    [N || {N,_} <- Nodes,
          node() /= N,
          pang <- [net_adm:ping(N)]];

ping(Config) ->
    Nodes = lists:droplast(?util:read_priv(Config, nodes)),
    [] = [{N,RC} || {N,S} <- Nodes,
                    RC <- [rpc:call(N, ?MODULE, ping, [{S,Nodes}])],
                    RC /= []].

%% start/1
%%
%% Start diameter services.

start(SvcName)
  when is_atom(SvcName) ->
    ok = diameter:start(),
    ok = diameter:start_service(SvcName, ?SERVICE((?L(SvcName))));

start(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [{N,RC} || {N,S} <- Nodes,
                    RC <- [rpc:call(N, ?MODULE, start, [S])],
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

connect({?SERVER, Config, [{Node, _} | _]}) ->
    if Node == node() ->  %% server0
            ?util:write_priv(Config, lref, {Node, ?util:listen(?SERVER, tcp)});
       true ->
            diameter_dist:attach([?SERVER])
    end,
    ok;

connect({?CLIENT, Config, _}) ->
    ?util:connect(?CLIENT, tcp, ?util:read_priv(Config, lref)),
    ok;

connect(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [{N,RC} || {N,S} <- Nodes,
                    RC <- [rpc:call(N, ?MODULE, connect, [{S, Config, Nodes}])],
                    RC /= ok].

%% stop/1
%%
%% Stop the slave nodes.

stop() ->
    [{timetrap, {seconds, 30*length(?NODES)}}].

stop(_Config) ->
    [] = [{N,E} || {N,_} <- ?NODES,
                   {error, _, _} = E <- [ct_slave:stop(N)]].

%% ===========================================================================
%% traffic testcases

%% send/1
%%
%% Send 100 requests and ensure the node name sent as User-Name isn't
%% the node terminating transport.

send(Config) ->
    send(Config, 100, dict:new()).

%% send/2

send(Config, 0, Dict) ->
    [{Server0, _} | _] = ?util:read_priv(Config, nodes) ,
    Node = atom_to_binary(Server0, utf8),
    {false, _} = {dict:is_key(Node, Dict), dict:to_list(Dict)};

send(Config, N, Dict) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'User-Name' = [ServerNode]}
        = send(Config, str(?LOGOUT)),
    true = is_binary(ServerNode),
    send(Config, N-1, dict:update_counter(ServerNode, 1, Dict)).

%% ===========================================================================

str(Cause) ->
    #diameter_base_STR{'Destination-Realm'   = ?REALM,
                       'Auth-Application-Id' = ?DICT:id(),
                       'Termination-Cause'   = Cause}.

%% send/2

send(Config, Req) ->
    {Node, _} = lists:last(?util:read_priv(Config, nodes)),
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
