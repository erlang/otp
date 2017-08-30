%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
%% Tests of transport_opt() length_errors.
%%

-module(diameter_length_SUITE).

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
         pick_peer/5,
         prepare_request/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).
-define(DICT, diameter_gen_base_rfc3588).

%% Config for diameter:start_service/2.
-define(SERVICE(Name),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [{127,0,0,1}]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {application, [{dictionary, ?DICT},
                        {module, ?MODULE},
                        {answer_errors, callback}]}]).

-define(SUCCESS,
        ?'DIAMETER_BASE_RESULT-CODE_SUCCESS').
-define(MISSING_AVP,
        ?'DIAMETER_BASE_RESULT-CODE_MISSING_AVP').
-define(INVALID_MESSAGE_LENGTH,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_MESSAGE_LENGTH').

-define(LOGOUT,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').

-define(GROUPS, [exit, handle, discard]).

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

origin(exit)    -> 0;
origin(handle)  -> 1;
origin(discard) -> 2;

origin(0) -> exit;
origin(1) -> handle;
origin(2) -> discard.

%% ===========================================================================

%% start/1

start(Config) ->
    Group = proplists:get_value(group, Config),
    ok = diameter:start_service(?SERVER, ?SERVICE(?L(Group))),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT)),
    LRef = ?util:listen(?SERVER,
                        tcp,
                        [{length_errors, Group}]),
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

%% Server transport exits on messages of insuffient length.
send(exit) ->
    %% Transport exit is followed by failover but there's only one
    %% transport to choose from.
    {error, failover} = call(4);

%% Server transport receives messages of insufficient length.
send(handle) ->
    %% Message Length too large: diameter_tcp flushes the request
    %% when no additional bytes arrive.
    #diameter_base_STA{'Result-Code' = ?INVALID_MESSAGE_LENGTH}
        = call(4),
    %% Another request answered as it should.
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(0),
    %% Message Length conveniently small: the trailing optional
    %% Origin-State-Id isn't included in the received request.
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(-12),
    %% Server receives Origin-State-Id AVP as the first 12 bytes of
    %% the next request: AVP <<Code:32, Flags:8, Len:24, Data:32>> is
    %% interpreted as header <<Version:8, Len:24, Flags:8, Code:24,
    %% ApplId: 32>>. In particular, the AVP Length 12 = 00001100 is
    %% interpreted as Command Flags, so R=0 and the request is
    %% interpreted as an unsolicited answer. Increase Message Length
    %% to have the server receive all bytes sent thusfar.
    {error, timeout}
        = call(12),
    %% Another request answered as it should.
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(0),
    %% Shorten Message Length so much that that the server doesn't
    %% receive the required Termination-Cause AVP.
    #diameter_base_STA{'Result-Code' = ?MISSING_AVP}
        = call(-24);

%% Server transport discards message of insufficient length.
send(discard) ->
    %% First request times out when the server discards it but a
    %% second succeeds since the transport remains up.
    {error, timeout}
        = call(4),
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(0);

send(Config) ->
    send(proplists:get_value(group, Config)).

%% ===========================================================================

call(Delta) ->
    diameter:call(?CLIENT,
                  ?DICT,
                  #diameter_base_STR
                  {'Termination-Cause' = ?LOGOUT,
                   'Auth-Application-Id' = ?DIAMETER_APP_ID_COMMON,
                   'Origin-State-Id' = [7]},
                  [{extra, [Delta]}]).

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/5

pick_peer([Peer], _, ?CLIENT, _State, _Delta) ->
    {ok, Peer}.

%% prepare_request/4

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, Delta) ->
    {send, resize(Delta, prepare(Pkt, Caps))}.

prepare(#diameter_packet{msg = Req0} = Pkt, Caps) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    Req = Req0#diameter_base_STR{'Session-Id' = diameter:session_id(OH),
                                 'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Destination-Realm' = DR},
    diameter_codec:encode(?DICT, Pkt#diameter_packet{msg = Req}).

resize(0, Pkt) ->
    Pkt;
resize(Delta, #diameter_packet{bin = Bin} = Pkt) ->
    Pkt#diameter_packet{bin = resize(Delta, Bin)};

resize(Delta, <<V, Len:24, T/binary>>) ->
    <<V, (Len + Delta):24, T/binary>>.

%% handle_answer/5

handle_answer(Pkt, _Req, ?CLIENT, _Peer, _Delta) ->
    Pkt#diameter_packet.msg.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer, _Delta) ->
    {error, Reason}.

%% handle_request/3

handle_request(Pkt, ?SERVER, {_Ref, Caps}) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _},
                   origin_state_id = {_,[Id]}}
        = Caps,
    answer(origin(Id),
           Pkt,
           #diameter_base_STA{'Result-Code' = ?SUCCESS,
                              'Session-Id' = diameter:session_id(OH),
                              'Origin-Host' = OH,
                              'Origin-Realm' = OR}).

answer(Group, #diameter_packet{errors = Es}, Ans) ->
    answer(Group, Es, Ans);

%% No errors: just answer.
answer(_, [], Ans) ->
    {reply, Ans};

%% Otherwise an invalid length should only reach the callback if
%% length_errors = handle.
answer(Group, [RC|_], Ans)
  when RC == ?INVALID_MESSAGE_LENGTH, Group == handle;
       RC /= ?INVALID_MESSAGE_LENGTH ->
    {reply, Ans}.
