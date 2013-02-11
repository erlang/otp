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
%% Tests of application_opt() request_errors.
%%

-module(diameter_3xxx_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         send/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).
-define(DICT, diameter_gen_base_rfc6733).

%% Config for diameter:start_service/2.
-define(SERVICE(Name, RequestErrors),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [{127,0,0,1}]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {application, [{dictionary, ?DICT},
                        {module, ?MODULE},
                        {answer_errors, callback},
                        {request_errors, RequestErrors}]}]).

-define(SUCCESS,     ?'DIAMETER_BASE_RESULT-CODE_SUCCESS').
-define(UNSUPPORTED, ?'DIAMETER_BASE_RESULT-CODE_COMMAND_UNSUPPORTED').

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').

-define(GROUPS, [answer_3xxx, callback]).
-define(L, atom_to_list).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, G} || G <- ?GROUPS].

groups() ->
    [{G, [], [start, send, stop]} || G <- ?GROUPS].

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

init_per_group(Group, Config) ->
    [{group, Group} | Config].

end_per_group(_, _) ->
    ok.

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

origin(answer_3xxx)   -> 0;
origin(callback) -> 1;

origin(0) -> answer_3xxx;
origin(1) -> callback.

%% ===========================================================================

%% start/1

start(Config) ->
    Group = proplists:get_value(group, Config),
    ok = diameter:start_service(?SERVER, ?SERVICE(?L(Group), Group)),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, callback)),
    LRef = ?util:listen(?SERVER, tcp),
    ?util:connect(?CLIENT,
                  tcp,
                  LRef,
                  [{capabilities, [{'Origin-State-Id', origin(Group)}]}]).

%% stop/1

stop(_Config) ->
    ok = diameter:remove_transport(?CLIENT, true),
    ok = diameter:remove_transport(?SERVER, true),
    ok = diameter:stop_service(?SERVER),
    ok = diameter:stop_service(?CLIENT).

%% send/1
%%
%% Send a unknown command and expect a different result depending on
%% whether or not the server gets a handle_request callback.

%% Server handle_request discards the request.
send(callback) ->
    {error, timeout} = call();

%% No handle_request, diameter answers.
send(answer_3xxx) ->
    #'diameter_base_answer-message'{'Result-Code' = ?UNSUPPORTED} = call();

send(Config) ->
    send(proplists:get_value(group, Config)).

%% ===========================================================================

call() ->
    diameter:call(?CLIENT,
                  ?DICT,
                  #diameter_base_STR
                  {'Termination-Cause' = ?LOGOUT,
                   'Auth-Application-Id' = ?DIAMETER_APP_ID_COMMON}).

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([Peer], _, ?CLIENT, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(#diameter_packet{msg = Req0} = Pkt0, ?CLIENT, {_Ref, Caps}) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    Req = Req0#diameter_base_STR{'Session-Id' = diameter:session_id(OH),
                                 'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Destination-Realm' = DR},
    #diameter_packet{bin = <<H:5/binary, 275:24, T/binary>>}
        = Pkt
        = diameter_codec:encode(?DICT, Pkt0#diameter_packet{msg = Req}),

    {send, Pkt#diameter_packet{bin = <<H/binary, 572:24, T/binary>>}}.

%% handle_answer/4

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    Pkt#diameter_packet.msg.

%% handle_error/4

handle_error(Reason, _Req, ?CLIENT, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_, ?SERVER, _) ->
    discard.
