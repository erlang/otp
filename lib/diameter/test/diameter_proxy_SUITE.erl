%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
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
%% Tests of traffic between three Diameter nodes connected as follows.
%%
%%   CLIENT.REALM ---- PROXY.REALM  ---- SERVER.REALM

-module(diameter_proxy_SUITE).

-export([suite/0,
         all/0,
         groups/0]).

%% testcases
-export([start/1,
         start_services/1,
         connect/1,
         send_flat_avps/1,
         send_grouped_avps/1,
         info/1,
         disconnect/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT,  "CLIENT.REALM").
-define(PROXY,  "PROXY.REALM").
-define(SERVER1, "SERVER1.REALM").
-define(SERVER2, "SERVER2.REALM").

-define(SERVICES, [?CLIENT,
                   ?PROXY,
                   ?SERVER1]).

-define(DICT_COMMON,  ?DIAMETER_DICT_COMMON).

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
                        {module, #diameter_callback{peer_up = false,
                                                    peer_down = false,
                                                    handle_error = false,
                                                    default = ?MODULE}},
                        {answer_errors, callback}]}]).

-define(SUCCESS, 2001).
-define(UNABLE_TO_DELIVER, 3002).

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(AUTHORIZE_ONLY, ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     start_services,
     connect,
     {group, all},
     %{group, all, [parallel]},
     disconnect,
     stop_services,
     stop].

groups() ->
    [{all, [], tc()}].

%% Traffic cases run when services are started and connections
%% established.
tc() ->
    [send_flat_avps,
     send_grouped_avps,
     info].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    [S1] = [server(N, ?DICT_COMMON) || N <- [?SERVER1]],
    [P1] = [server(N, ?DICT_COMMON) || N <- [?PROXY]],

    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),

    {save_config, [{?PROXY, [S1]},
                   {?CLIENT, [P1]}]}.

connect(Config) ->
    {_, Conns} = proplists:get_value(saved_config, Config),

    ?util:write_priv(Config,
                     "cfg",
                     lists:flatmap(fun({CN,Ss}) -> connect(CN, Ss) end,
                                   Conns)).

disconnect(Config) ->
    lists:foreach(fun({{CN,CR},{SN,SR}}) -> ?util:disconnect(CN,CR,SN,SR) end,
                  ?util:read_priv(Config, "cfg")).

stop_services(_Config) ->
    [] = [{H,T} || H <- ?SERVICES,
                   T <- [diameter:stop_service(H)],
                   T /= ok].

stop(_Config) ->
    ok = diameter:stop().

%% ----------------------------------------

server(Name, Dict) ->
    ok = diameter:start_service(Name, ?SERVICE(Name, Dict)),
    {Name, ?util:listen(Name, tcp)}.

connect(Name, Refs) ->
    [{{Name, ?util:connect(Name, tcp, LRef)}, T} || {_, LRef} = T <- Refs].

%% ===========================================================================
%% traffic testcases

send_flat_avps(_Config) ->
    send(?SERVER1, []).

send_grouped_avps(_Config) ->
    ProxyInfo = [#'diameter_base_Proxy-Info'{
                    'Proxy-Host' = "localhost",
                    'Proxy-State' = "some state"}],
    send(?SERVER1, ProxyInfo).

info(_Config) ->
    [] = ?util:info().

%% ===========================================================================

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

send(Server, ProxyInfo) ->
    Realm = realm(Server),
    Req = ['STR', {'Destination-Realm', Realm},
                  %{'Destination-Host', [Server]},
                  {'Termination-Cause', ?LOGOUT},
                  {'Auth-Application-Id', ?APP_ID},
                  % Sample grouped AVP
                  {'Proxy-Info', ProxyInfo}
           ],
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Origin-Host' = Server,
                       'Origin-Realm' = Realm}
    = call(Req, []).

call(Req, Opts) ->
    diameter:call(?CLIENT, ?APP_ALIAS, Req, Opts).

set([H|T], Vs) ->
    [H | Vs ++ T].

%% ===========================================================================
%% diameter callbacks

%% pick_peer/4

pick_peer([Peer | _], _, Svc, _State)
   when Svc == ?CLIENT;
        Svc == ?PROXY ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(Pkt, Svc, _Peer)
  when Svc == ?PROXY ->
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

%% A proxy must return Pkt.
handle_answer(Pkt, _Req, Svc, _Peer)
  when Svc == ?PROXY ->
    Pkt;

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_request/3

handle_request(Pkt, OH, {_Ref, #diameter_caps{origin_host = {OH,_}} = Caps})
  when OH /= ?CLIENT ->
    request(Pkt, Caps).

%% Other request to a proxy: send on to one of the servers in the
%% same realm.
request(_Pkt, #diameter_caps{origin_host = {OH, _}})
  when OH == ?PROXY ->
    {proxy, []};

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
    %% A proxy appends the identity of the peer that a request was
    %% received from to the Route-Record avp.
    [?CLIENT] = Route,
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}}.
