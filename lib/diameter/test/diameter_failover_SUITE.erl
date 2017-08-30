%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% Tests of traffic between seven Diameter nodes in four realms,
%% connected as follows.
%%
%%                  ----- SERVER1.REALM2 -----
%%                /                            \
%%               /  ----- SERVER2.REALM2 -----  \
%%              | /                            \ |
%%   CLIENT.REALM1 ------ SERVER3.REALM2 ------ CLIENT.REALM4
%%              | \                            / |
%%              |  \                          /  |
%%               \   ---- SERVER1.REALM3 -----  /
%%                \                            /
%%                  ----- SERVER2.REALM3 -----
%%

-module(diameter_failover_SUITE).

-export([suite/0,
         all/0]).

%% testcases
-export([start/1,
         start_services/1,
         connect/1,
         send_ok/1,
         send_nok/1,
         send_discard_1/1,
         send_discard_2/1,
         stop_services/1,
         empty/1,
         stop/1]).

%% diameter callbacks
-export([pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_error/4,
         handle_answer/4,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT1, "CLIENT.REALM1").
-define(CLIENT2, "CLIENT.REALM4").
-define(SERVER1, "SERVER1.REALM2").
-define(SERVER2, "SERVER2.REALM2").
-define(SERVER3, "SERVER3.REALM2").
-define(SERVER4, "SERVER1.REALM3").
-define(SERVER5, "SERVER2.REALM3").

-define(IS_CLIENT(Svc), Svc == ?CLIENT1; Svc == ?CLIENT2).

-define(CLIENTS, [?CLIENT1, ?CLIENT2]).
-define(SERVERS, [?SERVER1, ?SERVER2, ?SERVER3, ?SERVER4, ?SERVER5]).

-define(DICT_COMMON,  ?DIAMETER_DICT_COMMON).

-define(APP_ALIAS, the_app).
-define(APP_ID, ?DICT_COMMON:id()).

%% Config for diameter:start_service/2.
-define(SERVICE(Host),
        [{'Origin-Host', Host},
         {'Origin-Realm', realm(Host)},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Acct-Application-Id', [?APP_ID]},
         {application, [{alias, ?APP_ALIAS},
                        {dictionary, ?DICT_COMMON},
                        {module, #diameter_callback
                                  {peer_up = false,
                                   peer_down = false,
                                   default = ?MODULE}},
                        {answer_errors, callback}]}]).

-define(SUCCESS, 2001).

%% Value of Termination-Cause determines client/server behaviour.
-define(LOGOUT,   ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(MOVED,    ?'DIAMETER_BASE_TERMINATION-CAUSE_USER_MOVED').
-define(TIMEOUT,  ?'DIAMETER_BASE_TERMINATION-CAUSE_SESSION_TIMEOUT').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     start_services,
     connect,
     send_ok,
     send_nok,
     send_discard_1,
     send_discard_2,
     stop_services,
     empty,
     stop].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    Servers = [server(N) || N <- ?SERVERS],
    [] = [T || C <- ?CLIENTS,
               T <- [diameter:start_service(C, ?SERVICE(C))],
               T /= ok],

    {save_config, Servers}.

connect(Config) ->
    {start_services, Servers} = proplists:get_value(saved_config, Config),

    lists:foreach(fun(C) -> connect(C, Servers) end, ?CLIENTS).

stop_services(_Config) ->
    [] = [{H,T} || H <- ?CLIENTS ++ ?SERVERS,
                   T <- [diameter:stop_service(H)],
                   T /= ok].

%% Ensure transports have been removed from request table.
empty(_Config) ->
    [] = ets:tab2list(diameter_request).

stop(_Config) ->
    ok = diameter:stop().

%% ----------------------------------------

server(Name) ->
    ok = diameter:start_service(Name, ?SERVICE(Name)),
    {Name, ?util:listen(Name, tcp)}.

connect(Name, Refs) ->
    [{{Name, ?util:connect(Name, tcp, LRef)}, T} || {_, LRef} = T <- Refs].

%% ===========================================================================
%% traffic testcases

%% Send an STR and expect success after SERVER3 answers after a couple
%% of failovers.
send_ok(_Config) ->
    Req = #diameter_base_STR{'Destination-Realm' = realm(?SERVER1),
                             'Termination-Cause' = ?LOGOUT,
                             'Auth-Application-Id' = ?APP_ID},
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Origin-Host' = ?SERVER3}
        = call(?CLIENT1, Req).

%% Send an STR and expect failure when both servers fail.
send_nok(_Config) ->
    Req = #diameter_base_STR{'Destination-Realm' = realm(?SERVER4),
                             'Termination-Cause' = ?LOGOUT,
                             'Auth-Application-Id' = ?APP_ID},
    {failover, ?LOGOUT} = call(?CLIENT1, Req).

%% Send an STR and have prepare_retransmit discard it.
send_discard_1(_Config) ->
    Req = #diameter_base_STR{'Destination-Realm' = realm(?SERVER1),
                             'Termination-Cause' = ?TIMEOUT,
                             'Auth-Application-Id' = ?APP_ID},
    {rejected, ?TIMEOUT} = call(?CLIENT2, Req).
send_discard_2(_Config) ->
    Req = #diameter_base_STR{'Destination-Realm' = realm(?SERVER4),
                             'Termination-Cause' = ?MOVED,
                             'Auth-Application-Id' = ?APP_ID},
    {discarded, ?MOVED} = call(?CLIENT2, Req).

%% ===========================================================================

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

call(Svc, Req) ->
    diameter:call(Svc, ?APP_ALIAS, Req, [{filter, realm}]).

%% ===========================================================================
%% diameter callbacks

%% pick_peer/4

%% Choose a server other than SERVER3 or SERVER5 if possible.
pick_peer(Peers, _, Svc, _State)
  when ?IS_CLIENT(Svc) ->
    case lists:partition(fun({_, #diameter_caps{origin_host = {_, OH}}}) ->
                                 OH /= ?SERVER3 andalso OH /= ?SERVER5
                         end,
                         Peers)
    of
        {[], [Peer]} ->
            {ok, Peer};
        {[Peer | _], _} ->
            {ok, Peer}
    end.

%% prepare_request/3

prepare_request(Pkt, Svc, {_Ref, Caps})
  when ?IS_CLIENT(Svc) ->
    {send, prepare(Pkt, Caps)}.

prepare(#diameter_packet{msg = Req}, Caps) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    Req#diameter_base_STR{'Origin-Host' = OH,
                          'Origin-Realm' = OR,
                          'Session-Id' = diameter:session_id(OH)}.

%% prepare_retransmit/3

prepare_retransmit(#diameter_packet{header = H} = P, Svc, {_,_})
  when ?IS_CLIENT(Svc) ->
    #diameter_header{is_retransmitted = true} = H,  %% assert
    prepare(P).

prepare(#diameter_packet{msg = M} = P) ->
    case M#diameter_base_STR.'Termination-Cause' of
        ?LOGOUT  -> {send, P};
        ?MOVED   -> discard;
        ?TIMEOUT -> {discard, rejected}
    end.

%% handle_error/4

handle_error(Reason, Req, _, _) ->
    {Reason, Req#diameter_base_STR.'Termination-Cause'}.

%% handle_answer/4

handle_answer(Pkt, _Req, Svc, _Peer)
  when ?IS_CLIENT(Svc) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_request/3

%% Only SERVER3 actually answers.
handle_request(Pkt, ?SERVER3, {_, Caps}) ->
    #diameter_packet{header = #diameter_header{is_retransmitted = true},
                     msg = #diameter_base_STR{'Session-Id' = SId}}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,

    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}};

%% Others kill the transport to force failover.
handle_request(_, _, {TPid, _}) ->
    exit(TPid, kill),
    discard.
