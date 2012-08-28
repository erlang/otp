%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
%% Tests of traffic between six Diameter nodes in three realms,
%% connected as follows.
%%
%%                  ----- SERVER1.REALM2
%%                /
%%               /  ----- SERVER2.REALM2
%%              | /
%%   CLIENT.REALM1 ------ SERVER3.REALM2
%%              | \
%%              |  \
%%               \   ---- SERVER1.REALM3
%%                \
%%                  ----- SERVER2.REALM3
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
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([pick_peer/4,
         prepare_request/3,
         handle_answer/4,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT,  "CLIENT.REALM1").
-define(SERVER1, "SERVER1.REALM2").
-define(SERVER2, "SERVER2.REALM2").
-define(SERVER3, "SERVER3.REALM2").
-define(SERVER4, "SERVER1.REALM3").
-define(SERVER5, "SERVER2.REALM3").

-define(SERVICES, [?CLIENT, ?SERVER1, ?SERVER2, ?SERVER3, ?SERVER4, ?SERVER5]).

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
                        {module, #diameter_callback
                                  {peer_up = false,
                                   peer_down = false,
                                   handle_error = false,
                                   prepare_retransmit = false,
                                   default = ?MODULE}},
                        {answer_errors, callback}]}]).

-define(SUCCESS, 2001).

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_DIAMETER_LOGOUT').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     start_services,
     connect,
     send_ok,
     send_nok,
     stop_services,
     stop].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    S = [server(N, ?DICT_COMMON) || N <- tl(?SERVICES)],

    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),

    {save_config, [{?CLIENT, S}]}.

connect(Config) ->
    {_, Conns} = proplists:get_value(saved_config, Config),

    lists:foreach(fun({CN,Ss}) -> connect(CN, Ss) end, Conns).

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

%% Send an STR and expect success after SERVER3 answers after a couple
%% of failovers.
send_ok(_Config) ->
    Req = ['STR', {'Destination-Realm', realm(?SERVER1)},
                  {'Termination-Cause', ?LOGOUT},
                  {'Auth-Application-Id', ?APP_ID}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Origin-Host' = ?SERVER3}
        = call(Req, [{filter, realm}]).

%% Send an STR and expect failure when both servers fail.
send_nok(_Config) ->
    Req = ['STR', {'Destination-Realm', realm(?SERVER4)},
                  {'Termination-Cause', ?LOGOUT},
                  {'Auth-Application-Id', ?APP_ID}],
    {error, failover} = call(Req, [{filter, realm}]).

%% ===========================================================================

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

call(Req, Opts) ->
    diameter:call(?CLIENT, ?APP_ALIAS, Req, Opts).

set([H|T], Vs) ->
    [H | Vs ++ T].

%% ===========================================================================
%% diameter callbacks

%% pick_peer/4

%% Choose a server other than SERVER3 or SERVER5 if possible.
pick_peer(Peers, _, ?CLIENT, _State) ->
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

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}) ->
    {send, prepare(Pkt, Caps)}.

prepare(#diameter_packet{msg = Req}, Caps) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR}]).

%% handle_answer/4

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_request/3

%% Only SERVER3 actually answers.
handle_request(Pkt, ?SERVER3, {_, Caps}) ->
    #diameter_packet{msg = #diameter_base_STR{'Session-Id' = SId,
                                              'Origin-Host' = ?CLIENT}}
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
