%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         %% The test cases
         traffic/1
        ]).

%% rpc calls
-export([ping/1,
         start/1,
         stop/1,
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

-include("diameter_util.hrl").


%% ===========================================================================

-define(DL(F),    ?DL(F, [])).
-define(DL(F, A), ?LOG("DDISTRS", F, A)).

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
-define(NODES, [{server,  ?SERVER},
                {client0, ?CLIENT},
                {client1, ?CLIENT},
                {client2, ?CLIENT}]).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [traffic].


init_per_suite(Config) ->
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?DUTIL:end_per_suite(Config).


traffic(Config) ->
    ?DL("traffic -> entry"),
    Factor = dia_factor(Config),
    Res = do_traffic(Factor),
    ?DL("traffic -> done when"
        "~n   Res: ~p", [Res]),
    Res.


%% ===========================================================================

run() ->
    [] = ?RUN([{fun() -> do_traffic(1) end, 60000}]).
    %% process for linked peers to die with

%% traffic/0

do_traffic(Factor) ->
    ?DL("do_traffic -> make sure we have distro"),
    true = is_alive(),  %% need distribution for peer nodes
    ?DL("do_traffic -> get nodes"),
    Nodes = get_nodes(),
    ?DL("do_traffic -> ping nodes"),
    [] = ping(Nodes),  %% drop client node
    ?DL("do_traffic -> start nodes"),
    [] = start(Nodes),
    ?DL("do_traffic -> connect nodes"),
    [_] = connect(Nodes),
    ?DL("do_traffic -> send (to) nodes"),
    [] = send(Nodes, Factor),
    ?DL("do_traffic -> stop nodes"),
    [] = stop(Nodes),
    ?DL("do_traffic -> done"),
    ok.

%% get_nodes/0
%%
%% Start four nodes;
%%   - one to implement a Diameter server,
%%   - three to implement a client.

get_nodes() ->
    Here = filename:dirname(code:which(?MODULE)),
    Ebin = filename:join([Here, "..", "ebin"]),
    Args = ["-pa", Here, Ebin],
    [{N,S} || {M,S} <- ?NODES, N <- [start(M, Args)]].

start(Name, Args) ->
    {ok, _, Node} = ?DUTIL:peer(#{name => Name, args => Args}),
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

%% stop/1
%%
%% Stop diameter services.

stop(SvcName)
  when is_atom(SvcName) ->
    ok = diameter:stop_service(SvcName),
    ok = diameter:stop();

stop(Nodes) ->
    [{N,RC} || {N,S} <- Nodes,
               RC <- [rpc:call(N, ?MODULE, stop, [S])],
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
    [_LRef = ?LISTEN(?SERVER, tcp)];

connect({?CLIENT, [{Node, _} | _], [LRef] = Acc}) ->
    ?CONNECT(?CLIENT, tcp, {Node, LRef}),
    Acc;

connect(Nodes) ->
    lists:foldl(fun({N,S}, A) ->
                        rpc:call(N, ?MODULE, connect, [{S, Nodes, A}])
                end,
                [],
                Nodes).

%% ===========================================================================

%% send/2

send(Nodes, Factor) ->
    ?RUN([[fun send/3, Nodes, T, Factor]
          || T <- [local, remote, timeout, failover]]).

%% send/3

%% Send a request from the first client node, using a the local
%% transport.
send(Nodes, local, Factor) ->
    ?DL("send(local) -> entry - expect success (~p)", [?SUCCESS]),
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Nodes, 0, str(?LOGOUT), Factor),
    ?DL("send(local) -> success (=success)"),
    ok;

%% Send a request from the first client node, using a transport on the
%% another node.
send(Nodes, remote, Factor) ->
    ?DL("send(remote) -> entry - expect success (~p)", [?SUCCESS]),
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send(Nodes, 1, str(?LOGOUT), Factor),
    ?DL("send(remote) -> success (=success)"),
    ok;

%% Send a request that the server discards.
send(Nodes, timeout, Factor) ->
    ?DL("send(timeout) -> entry - expect timeout"),
    {error, timeout} = send(Nodes, 1, str(?TIMEOUT), Factor),
    ?DL("send(timeout) -> success (=timeout)"),
    ok;

%% Send a request that causes the server to take the transport down.
send(Nodes, failover, Factor) ->
    ?DL("send(failover) -> entry - expect busy (~p)", [?BUSY]),
    #'diameter_base_answer-message'{'Result-Code' = ?BUSY}
        = send(Nodes, 2, str(?MOVED), Factor),
    ?DL("send(failover) -> success (=busy)"),
    ok.


%% ===========================================================================

str(Cause) ->
    #diameter_base_STR{'Destination-Realm'   = ?REALM,
                       'Auth-Application-Id' = ?DICT:id(),
                       'Termination-Cause'   = Cause}.

%% send/4

%% There is a "bug" in diameter, which can cause this function to return
%% {error, timeout} even though only a fraction on the time has expired.
%% This is because the timer has in fact *not* expired. Instead what
%% has happened is the transport process has died and the selection
%% of a new transport fails (I think its a race causing the pick_peer
%% to return the same tranport process), at that error is converted to
%% a timeout error.
%% So, if this call returns {error, timeout} but only a fraction of the
%% time has passed we skip instead!
send([_, {Node, _} | _], Where, Req, Factor) ->
    ?DL("send -> make rpc call to node ~p", [Node]),
    case rpc:call(Node, ?MODULE, call, [{Where, Req, Factor}]) of
        {{error, timeout} = Result, T1, T2, Timeout}
          when is_integer(T1) andalso is_integer(T2) ->
            TDiff = T2 - T1,
            ?DL("request completed:"
                "~n   Time:    ~w msec"
                "~n   Timeout: ~w msec"
                "~n   Result:  ~p", [TDiff, Timeout, Result]),
            if
                (TDiff < 100) ->
                    exit({skip, {invalid_timeout, TDiff, Timeout}});
                true ->
                    Result
            end;
        {Result, T1, T2, Timeout} when is_integer(T1) andalso is_integer(T2) ->
            ?DL("request completed:"
                "~n   Time:    ~w msec"
                "~n   Timeout: ~w msec"
                "~n   Result:  ~p", [T2-T1, Timeout, Result]),
            Result;
        {badrpc, Reason} ->
            ?DL("rpc failed:"
                "~n   Reason: ~p", [Reason]),
            ct:fail({rpc_call_failed, Node, Where, Req, Reason})
    end.

%% call/1

call({Where, Req, Factor}) ->
    Timeout = timeout(Factor),
    ?DL("call -> make diameter call with"
        "~n   (own) Node: ~p"
        "~n   Where:      ~p"
        "~n   Req:        ~p"
        "~nwhen"
        "~n   Timeout:    ~w (~w)", [node(), Where, Req, Timeout, Factor]),
    T1 = ?TS(),
    Result = diameter:call(?CLIENT, ?DICT, Req, [{extra, [{Where, sname()}]},
                                                 {timeout, Timeout}]),
    T2 = ?TS(),
    ?DL("call -> diameter call ended with"
        "~n   Result: ~p"
        "~nwhen"
        "~n   T2-T1:  ~w (~w - ~w)", [Result, T2 - T1, T2, T1]),
    {Result, T1, T2, Timeout}.

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

handle_error(Reason, _Req, ?CLIENT = Svc, _Peer, {_, client0}) ->
    ?DL("~w(~p) -> entry with"
        "~n   Reason: ~p", [?FUNCTION_NAME, Svc, Reason]),
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


%% ===========================================================================

-define(CALL_TO_DEFAULT, 5000).
timeout(Factor) when (Factor > 0) andalso (Factor =< 20) ->
    (Factor - 1) * 500 + ?CALL_TO_DEFAULT;
timeout(Factor) when (Factor > 0) ->
    3*?CALL_TO_DEFAULT. % Max at 15 seconds


%% ===========================================================================

dia_factor(Config) ->
    config_lookup(?FUNCTION_NAME, Config).

config_lookup(Key, Config) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, Config),
    Value.
