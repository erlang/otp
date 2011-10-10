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
%% Tests of traffic between seven Diameter nodes connected as follows.
%%
%%                                     --- SERVER1.REALM2
%%                                   /
%%                  ---- RELAY.REALM2 ---- SERVER2.REALM2
%%                /        |
%%   CLIENT.REALM1         |
%%                \        |
%%                  ---- RELAY.REALM3 ---- SERVER1.REALM3
%%                                   \
%%                                     --- SERVER2.REALM3
%%

-module(diameter_relay_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2]).

%% testcases
-export([start/1,
         start_services/1,
         connect/1,
         send1/1,
         send2/1,
         send3/1,
         send4/1,
         send_loop/1,
         send_timeout_1/1,
         send_timeout_2/1,
         disconnect/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-ifdef(DIAMETER_CT).
-include("diameter_gen_base_rfc3588.hrl").
-else.
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-endif.

-include_lib("diameter/include/diameter.hrl").
-include("diameter_ct.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT,  "CLIENT.REALM1").
-define(RELAY1,  "RELAY.REALM2").
-define(SERVER1, "SERVER1.REALM2").
-define(SERVER2, "SERVER2.REALM2").
-define(RELAY2,  "RELAY.REALM3").
-define(SERVER3, "SERVER1.REALM3").
-define(SERVER4, "SERVER2.REALM3").

-define(SERVICES, [?CLIENT,
                   ?RELAY1, ?RELAY2,
                   ?SERVER1, ?SERVER2, ?SERVER3, ?SERVER4]).

-define(DICT_COMMON,  ?DIAMETER_DICT_COMMON).
-define(DICT_RELAY,   ?DIAMETER_DICT_RELAY).

-define(APP_ALIAS, the_app).
-define(APP_ID, ?DICT_COMMON:id()).

%% Config for diameter:start_service/2.
-define(SERVICE(Host, Dict),
        [{'Origin-Host', Host},
         {'Origin-Realm', realm(Host)},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Acct-Application-Id', [Dict:id()]},
         {application, [{alias, ?APP_ALIAS},
                        {dictionary, Dict},
                        {module, ?MODULE},
                        {answer_errors, callback}]}]).

%% Config for diameter:add_transport/2. In the listening case, listen
%% on a free port that we then lookup using the implementation detail
%% that diameter_tcp registers the port with diameter_reg.
-define(CONNECT(PortNr),
        {connect, [{transport_module, diameter_tcp},
                   {transport_config, [{raddr, ?ADDR},
                                       {rport, PortNr},
                                       {ip, ?ADDR},
                                       {port, 0}]}]}).
-define(LISTEN,
        {listen, [{transport_module, diameter_tcp},
                  {transport_config, [{ip, ?ADDR}, {port, 0}]}]}).

-define(SUCCESS, 2001).
-define(LOOP_DETECTED, 3005).
-define(UNABLE_TO_DELIVER, 3002).

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_DIAMETER_LOGOUT').
-define(AUTHORIZE_ONLY, ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').

-define(A, list_to_atom).
-define(L, atom_to_list).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [start, start_services, connect]
        ++ tc()
        ++ [{group, all},
            disconnect,
            stop_services,
            stop].

groups() ->
    [{all, [parallel], tc()}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

%% Traffic cases run when services are started and connections
%% established.
tc() ->
    [send1,
     send2,
     send3,
     send4,
     send_loop,
     send_timeout_1,
     send_timeout_2].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    S = [server(N, ?DICT_COMMON) || N <- [?SERVER1,
                                          ?SERVER2,
                                          ?SERVER3,
                                          ?SERVER4]],
    R = [server(N, ?DICT_RELAY) || N <- [?RELAY1, ?RELAY2]],

    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),

    {save_config, S ++ R}.

connect(Config) ->
    {_, [S1,S2,S3,S4,R1,R2] = SR} = proplists:get_value(saved_config, Config),

    true = diameter:subscribe(?RELAY1),
    true = diameter:subscribe(?RELAY2),
    true = diameter:subscribe(?CLIENT),

    [R1S1,R1S2] = connect(?RELAY1, [S1,S2]),
    [R2S3,R2S4] = connect(?RELAY2, [S3,S4]),
    [CR1,CR2]   = connect(?CLIENT, [R1,R2]),

    R1R2 = connect(?RELAY1, R2),

    ?util:write_priv(Config, "cfg", SR ++ [R1S1,R1S2,R2S3,R2S4,CR1,CR2,R1R2]).

%% Remove the client transports and expect the corresponding server
%% transport to go down.
disconnect(Config) ->
    [S1,S2,S3,S4,R1,R2,R1S1,R1S2,R2S3,R2S4,CR1,CR2,R1R2]
        = ?util:read_priv(Config, "cfg"),

    [?CLIENT | Svcs] = ?SERVICES,
    [] = [{S,T} || S <- Svcs, T <- [diameter:subscribe(S)], T /= true],

    disconnect(?RELAY1, S1, R1S1),
    disconnect(?RELAY1, S2, R1S2),
    disconnect(?RELAY2, S3, R2S3),
    disconnect(?RELAY2, S4, R2S4),
    disconnect(?CLIENT, R1, CR1),
    disconnect(?CLIENT, R2, CR2),
    disconnect(?RELAY1, R2, R1R2).

stop_services(_Config) ->
    [] = [{H,T} || H <- ?SERVICES,
                   T <- [diameter:stop_service(H)],
                   T /= ok].

stop(_Config) ->
    ok = diameter:stop().

%% ===========================================================================
%% traffic testcases

%% Send an STR intended for a specific server and expect success.
send1(_Config) ->
    call(?SERVER1).
send2(_Config) ->
    call(?SERVER2).
send3(_Config) ->
    call(?SERVER3).
send4(_Config) ->
    call(?SERVER4).

%% Send an ASR that loops between the relays and expect the loop to
%% be detected.
send_loop(_Config) ->
    Req = ['ASR', {'Destination-Realm', realm(?SERVER1)},
                  {'Destination-Host', ?SERVER1},
                  {'Auth-Application-Id', ?APP_ID}],
    #'diameter_base_answer-message'{'Result-Code' = ?LOOP_DETECTED}
        = call(Req, [{filter, realm}]).

%% Send a RAR that is incorrectly routed and then discarded and expect
%% different results depending on whether or not we or the relay
%% timeout first.
send_timeout_1(_Config) ->
    #'diameter_base_answer-message'{'Result-Code' = ?UNABLE_TO_DELIVER}
        = send_timeout(7000).
send_timeout_2(_Config) ->
    {error, timeout} = send_timeout(3000).

send_timeout(Tmo) ->
    Req = ['RAR', {'Destination-Realm', realm(?SERVER1)},
                  {'Destination-Host', ?SERVER1},
                  {'Auth-Application-Id', ?APP_ID},
                  {'Re-Auth-Request-Type', ?AUTHORIZE_ONLY}],
    call(Req, [{filter, realm}, {timeout, Tmo}]).

%% ===========================================================================

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

server(Host, Dict) ->
    ok = diameter:start_service(Host, ?SERVICE(Host, Dict)),
    {ok, LRef} = diameter:add_transport(Host, ?LISTEN),
    {LRef, portnr(LRef)}.

portnr(LRef) ->
    portnr(LRef, 20).

portnr(LRef, N)
  when 0 < N ->
    case diameter_reg:match({diameter_tcp, listener, {LRef, '_'}}) of
        [{T, _Pid}] ->
            {_, _, {LRef, {_Addr, LSock}}} = T,
            {ok, PortNr} = inet:port(LSock),
            PortNr;
        [] ->
            receive after 50 -> ok end,
            portnr(LRef, N-1)
    end.

connect(Host, {_LRef, PortNr}) ->
    {ok, Ref} = diameter:add_transport(Host, ?CONNECT(PortNr)),
    ok = receive
             #diameter_event{service = Host,
                             info = {up, Ref, _, _, #diameter_packet{}}} ->
                 ok
         after 2000 ->
                 false
         end,
    Ref;
connect(Host, Ports) ->
    [connect(Host, P) || P <- Ports].

disconnect(Client, {LRef, _PortNr}, CRef) ->
    ok = diameter:remove_transport(Client, CRef),
    ok = receive #diameter_event{info = {down, LRef, _, _}} -> ok
         after 2000 -> false
         end.

call(Server) ->
    Realm = realm(Server),
    Req = ['STR', {'Destination-Realm', Realm},
                  {'Destination-Host', [Server]},
                  {'Termination-Cause', ?LOGOUT},
                  {'Auth-Application-Id', ?APP_ID}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Origin-Host' = Server,
                       'Origin-Realm' = Realm}
        = call(Req, [{filter, realm}]).

call(Req, Opts) ->
    diameter:call(?CLIENT, ?APP_ALIAS, Req, Opts).
    
set([H|T], Vs) ->
    [H | Vs ++ T].

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([Peer | _], _, Svc, _State)
  when Svc == ?RELAY1;
       Svc == ?RELAY2;
       Svc == ?CLIENT->
    {ok, Peer}.

%% prepare_request/3

prepare_request(Pkt, Svc, _Peer)
  when Svc == ?RELAY1;
       Svc == ?RELAY2 ->
    {send, Pkt};

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}) ->
    {send, prepare(Pkt, Caps)}.

prepare(#diameter_packet{msg = Req}, Caps) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR}]).

%% prepare_retransmit/3

prepare_retransmit(_Pkt, false, _Peer) ->
    discard.

%% handle_answer/4

%% A relay must return Pkt.
handle_answer(Pkt, _Req, Svc, _Peer)
  when Svc == ?RELAY1;
       Svc == ?RELAY2 ->
    Pkt;

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_error/4

handle_error(Reason, _Req, _Svc, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(Pkt, OH, {_Ref, #diameter_caps{origin_host = {OH,_}} = Caps})
  when OH /= ?CLIENT ->
    request(Pkt, Caps).

%% RELAY1 routes any ASR or RAR to RELAY2 ...
request(#diameter_packet{header = #diameter_header{cmd_code = C}},
        #diameter_caps{origin_host = {?RELAY1, _}})
  when C == 274;   %% ASR
       C == 258 -> %% RAR
    {relay, [{filter, {realm, realm(?RELAY2)}}]};

%% ... which in turn routes it back. Expect diameter to either answer
%% either with DIAMETER_LOOP_DETECTED/DIAMETER_UNABLE_TO_COMPLY.
request(#diameter_packet{header = #diameter_header{cmd_code = 274}},
        #diameter_caps{origin_host = {?RELAY2, _}}) ->
    {relay, [{filter, {host, ?RELAY1}}]};
request(#diameter_packet{header = #diameter_header{cmd_code = 258}},
        #diameter_caps{origin_host = {?RELAY2, _}}) ->
    discard;

%% Other request to a relay: send on to one of the servers in the
%% same realm.
request(_Pkt, #diameter_caps{origin_host = {OH, _}})
  when OH == ?RELAY1;
       OH == ?RELAY2 ->
    {relay, [{filter, {all, [host, realm]}}]};

%% Request received by a server: answer.
request(#diameter_packet{msg = #diameter_base_STR{'Session-Id' = SId,
                                                  'Origin-Host' = Host,
                                                  'Origin-Realm' = Realm,
                                                  'Route-Record' = Route}},
        #diameter_caps{origin_host  = {OH, _},
                       origin_realm = {OR, _}}) ->
    %% The request should have the Origin-Host/Realm of the original
    %% sender.
    R = realm(?CLIENT),
    {?CLIENT, R} = {Host, Realm},
    %% A relay appends the identity of the peer that a request was
    %% received from to the Route-Record avp.
    [?CLIENT] = Route,
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}}.
