%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
%% Tests of traffic between two Diameter nodes, the client being
%% spread across three Erlang nodes.
%%

-module(diameter_distribution_SUITE).

-export([suite/0,
         all/0]).

%% testcases
-export([start/1,
         ping/1,
         connect/1,
         send_local/1,
         send_remote/1,
         send_timeout/1,
         send_failover/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_answer/5,
         handle_error/5,
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
         {share_peers, true},
         {use_shared_peers, true},
         {restrict_connections, false},
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
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     ping,
     connect,
     send_local,
     send_remote,
     send_timeout,
     send_failover,
     stop].

%% ===========================================================================
%% start/stop testcases

%% start/1
%%
%% Start three slave nodes, one that implement a Diameter server and
%% two that implement a client.

start(SvcName)
  when is_atom(SvcName) ->
    ok = diameter:start(),
    ok = diameter:start_service(SvcName, ?SERVICE((?L(SvcName))));

start(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    Nodes = [{N, Svcname} || {Nodename, Svcname} <- ?NODES,
                             N <- [slave(Nodename, Dir)]],
    [] = [RC || {N,S} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, start, [S])],
                RC /= ok],
    ?util:write_priv(Config, nodes, Nodes).

slave(Name, Dir) ->
    {ok, Node} = ct_slave:start(Name),
    ok = rpc:call(Node,
                  code,
                  add_pathsa,
                  [[Dir, filename:join([Dir, "..", "ebin"])]]),
    Node.

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

%% ping/1
%%
%% Ensure the client nodes are connected since the sharing of
%% transports is only between connected nodes.

ping({?SERVER, _Nodes}) ->
    ok;

ping({?CLIENT, Nodes}) ->
    {_, []} = {node(), [N || {N,_} <- Nodes,
                             node() /= N,
                             pang <- [net_adm:ping(N)]]},
    ok;

ping(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [RC || {N,S} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, ping, [{S, Nodes}])],
                RC /= ok].

%% connect/1
%%
%% Establish one connection to the server from each of the client
%% nodes.

connect({?SERVER, Config}) ->
    ?util:write_priv(Config, lref, {node(), ?util:listen(?SERVER, tcp)}),
    ok;

connect({?CLIENT, Config}) ->
    ?util:connect(?CLIENT, tcp, ?util:read_priv(Config, lref)),
    ok;

connect(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [RC || {N,S} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, connect, [{S,Config}])],
                RC /= ok].

%% stop/1
%%
%% Stop the slave nodes.

stop(Name)
  when is_atom(Name) ->
    {ok, _Node} = ct_slave:stop(Name),
    ok;

stop(_Config) ->
    [] = [RC || {N,_} <- ?NODES,
                RC <- [stop(N)],
                RC /= ok].

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
    [_, {Node, _} | _] = ?util:read_priv(Config, nodes) ,
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

pick_peer([LP], [_, _], ?CLIENT, _State, {local, client0}) ->
    {ok, LP};

pick_peer([_], [RP | _], ?CLIENT, _State, {remote, client0}) ->
    {ok, RP};

pick_peer([LP], [], ?CLIENT, _State, {remote, client0}) ->
    {ok, LP}.

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
    if N == 0 ->  %% sent from the originating node ...
            {protocol_error, ?BUSY};
       true ->    %% ... or through a remote node: force failover
            exit(TPid, kill),
            discard
    end;

request(#diameter_base_STR{'Session-Id' = SId}, {_, Caps}) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}}.
