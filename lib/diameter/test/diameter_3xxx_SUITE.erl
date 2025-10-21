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
%% Tests of application_opt() request_errors. There's some overlap
%% between this suite and the traffic suite but latter exercises more
%% config.
%%

-module(diameter_3xxx_SUITE).

%% testcases, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         %% The test cases
         answer/1,
         answer_3xxx/1,
         callback/1
        ]).

%% internal
-export([send_unknown_application/2,
         send_unknown_command/2,
         send_ok/2,
         send_invalid_hdr_bits/2,
         send_missing_avp/2,
         send_ignore_missing_avp/2,
         send_5xxx_missing_avp/2,
         send_double_error/2,
         send_3xxx/2,
         send_5xxx/2]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").
%% Use the fact that STR/STA is identical in RFC's 3588 and 6733.

-include("diameter_util.hrl").


%% ===========================================================================

-define(L, atom_to_list).
-define(A, list_to_atom).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").

-define(ERRORS, [answer, answer_3xxx, callback]).
-define(RFCS, [rfc3588, rfc6733]).

-define(DICT(RFC), ?A("diameter_gen_base_" ++ ?L(RFC))).
-define(DICT, ?DICT(rfc6733)).

-define(COMMON, ?DIAMETER_APP_ID_COMMON).

%% Config for diameter:start_service/2.
-define(SERVICE(Name, Errors, RFC),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [{127,0,0,1}]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?COMMON]},
         {application, [{dictionary, ?DICT(RFC)},
                        {module, ?MODULE},
                        {answer_errors, callback},
                        {request_errors, Errors}]}]).

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').

-define(XL(F),    ?XL(F, [])).
-define(XL(F, A), ?LOG("D3XS", F, A)).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [{group, rfc3588}, {group, rfc6733}].

groups() ->
    [{rfc3588, [], [answer, answer_3xxx, callback]},
     {rfc6733, [], [answer, answer_3xxx, callback]}].

init_per_suite(Config) ->
    ?XL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?XL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ?XL("init_per_group(~w) -> entry with"
        "~n   Config: ~p", [GroupName, Config]),
    [{rfc, GroupName} | Config].

end_per_group(GroupName, _Config) ->
    ?XL("end_per_group(~w) -> entry and done", [GroupName]),
    ok.

%% This test case can take a *long* time, so if the machine is too slow, skip
init_per_testcase(Case, Config) ->
    ?XL("init_per_testcase(~w) -> entry with"
        "~n   Config: ~p"
        "~n   => check factor", [Case, Config]),
    Key = dia_factor,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Factor}} when (Factor > 10) ->
            ?XL("init_per_testcase(~w) -> Too slow (~w) => SKIP",
                [Case, Factor]),
            {skip, {machine_too_slow, Factor}};
        _ ->
            ?XL("init_per_testcase(~w) -> run test", [Case]),
            ok = diameter:start(),
            Config
    end.

end_per_testcase(Case, _Config) ->
    ?XL("end_per_testcase(~w) -> entry and done", [Case]),
    ok = diameter:stop().

tc() ->
    [send_unknown_application,
     send_unknown_command,
     send_ok,
     send_invalid_hdr_bits,
     send_missing_avp,
     send_ignore_missing_avp,
     send_5xxx_missing_avp,
     send_double_error,
     send_3xxx,
     send_5xxx].

% %% run/0

run() ->
    ?XL("run -> entry"),
    {ok, Logger} = ?DEL_START(),
    try
        ok = diameter:start(),
        [apply(?MODULE, Errors, [[{logger, Logger}, {rfc, RFC}]]) ||
            Errors <- ?ERRORS, RFC <- ?RFCS]
    after
        ?DEL_STOP(),
        ok = diameter:stop()
    end.

%% ===========================================================================

%% start_service/1

start_service(Errors, RFC) ->
    ?XL("start_service -> entry with"
        "~n   Errors: ~p"
        "~n   RFC:    ~p", [Errors, RFC]),
    Name = ?L(Errors) ++ "," ++ ?L(RFC),

    ?XL("start_service -> subscribe to 'server' (diameter) events"),
    ok = ?DEL_REG(?SERVER),
    ?XL("start_service -> subscribe to 'client' (diameter) events"),
    ok = ?DEL_REG(?CLIENT),

    ?XL("start_service -> start service 'server' (~p)", [Name]),
    ok = diameter:start_service(?SERVER, ?SERVICE(Name, Errors, RFC)),
    ?XL("start_service -> start service 'client'"),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT,
                                                  callback,
                                                  rfc6733)),
    ?XL("start_service -> (server) listen"),
    LRef = ?LISTEN(?SERVER, tcp),
    ?XL("start_service -> (client) connect"),
    ?CONNECT(?CLIENT, tcp, LRef),
    ok.

%% stop_service/0

stop_service() ->
    ?XL("stop_service -> remove 'client' transport"),
    ok = diameter:remove_transport(?CLIENT, true),
    ?XL("stop_service -> remove 'server' transport"),
    ok = diameter:remove_transport(?SERVER, true),
    ?XL("stop_service -> stop service 'server'"),
    ok = diameter:stop_service(?SERVER),
    ?XL("stop_service -> stop service 'client'"),
    ok = diameter:stop_service(?CLIENT),

    ?XL("stop_service -> unsubscribe from 'server' (diameter) events"),
    ok = ?DEL_UNREG(?SERVER),
    ?XL("stop_service -> unsubscribe from 'client' (diameter) events"),
    ok = ?DEL_UNREG(?CLIENT),

    ?XL("stop_service -> done"),
    ok.

%% ===========================================================================

%% execute_testcase/2

execute_testcase(Errors, RFC) ->
    ?XL("execute_testcase -> entry with"
        "~n   Error: ~p"
        "~n   RFC:   ~p", [Errors, RFC]),
    ?XL("execute_testcase -> start service"),
    ok = start_service(Errors, RFC),
    try
        [apply(?MODULE, F, [Errors, RFC]) || F <- tc()],
        counters(Errors, RFC)
    after
        ?XL("execute_testcase -> stop service"),
        ok = stop_service()
    end.

%% ===========================================================================

counters(Errors, RFC) ->
    [counters(K, S, Errors, RFC) || K <- [statistics, transport, connections],
                                    S <- [?CLIENT, ?SERVER]].

counters(Key, Svc, Errors, RFC) ->
    counters(Key, Svc, Errors, RFC, [_|_] = diameter:service_info(Svc, Key)).

counters(statistics, Svc, Errors, Rfc, L) ->
    [{P, Stats}] = L,
    true = is_pid(P),
    stats(Svc, Errors, Rfc, lists:sort(Stats));

counters(_, _, _, _, _) ->
    todo.

%% ===========================================================================

stats(?CLIENT, E, rfc3588, L)
  when E == answer;
       E == answer_3xxx ->
    [{{{unknown,0},recv},2},
     {{{0,257,0},recv},1},
     {{{0,257,1},send},1},
     {{{0,275,0},recv},6},
     {{{0,275,1},send},10},
     {{{unknown,0},recv,{'Result-Code',3001}},1},
     {{{unknown,0},recv,{'Result-Code',3007}},1},
     {{{0,257,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',3008}},2},
     {{{0,275,0},recv,{'Result-Code',3999}},1},
     {{{0,275,0},recv,{'Result-Code',5002}},1},
     {{{0,275,0},recv,{'Result-Code',5005}},1}]
        = L;

stats(?SERVER = Svc, E, rfc3588 = RFC, L)
  when E == answer;
       E == answer_3xxx ->
    %% [{{{unknown,0},send},2},
    %%  {{{unknown,1},recv},1},
    %%  {{{0,257,0},send},1},
    %%  {{{0,257,1},recv},1},
    %%  {{{0,275,0},send},6},
    %%  {{{0,275,1},recv},8},
    %%  {{{unknown,0},send,{'Result-Code',3001}},1},
    %%  {{{unknown,0},send,{'Result-Code',3007}},1},
    %%  {{{unknown,1},recv,error},1},
    %%  {{{0,257,0},send,{'Result-Code',2001}},1},
    %%  {{{0,275,0},send,{'Result-Code',2001}},1},
    %%  {{{0,275,0},send,{'Result-Code',3008}},2},
    %%  {{{0,275,0},send,{'Result-Code',3999}},1},
    %%  {{{0,275,0},send,{'Result-Code',5002}},1},
    %%  {{{0,275,0},send,{'Result-Code',5005}},1},
    %%  {{{0,275,1},recv,error},5}]
    %% = L;
    ?XL("~w(~p, ~w) -> (attempt to) verify answer: "
        "~n   E: ~p"
        "~n   L: ~p", [?FUNCTION_NAME, Svc, RFC, E, L]),
    Expected = [{{{unknown,0},send},2},
                {{{unknown,1},recv},1},
                {{{0,257,0},send},1},
                {{{0,257,1},recv},1},
                {{{0,275,0},send},6},
                {{{0,275,1},recv},8},
                {{{unknown,0},send,{'Result-Code',3001}},1},
                {{{unknown,0},send,{'Result-Code',3007}},1},
                {{{unknown,1},recv,error},1},
                {{{0,257,0},send,{'Result-Code',2001}},1},
                {{{0,275,0},send,{'Result-Code',2001}},1},
                {{{0,275,0},send,{'Result-Code',3008}},2},
                {{{0,275,0},send,{'Result-Code',3999}},1},
                {{{0,275,0},send,{'Result-Code',5002}},1},
                {{{0,275,0},send,{'Result-Code',5005}},1},
                {{{0,275,1},recv,error},5}],
    case L of
        Expected ->
            ?XL("~w(~w) -> ok", [?FUNCTION_NAME, RFC]),
            L;
        _ ->
            ?XL("~w(~w, ~w) -> wrong: "
                "~n   L -- Expected: ~p"
                "~n   Expected -- L: ~p",
                [?FUNCTION_NAME, Svc, RFC, L -- Expected, Expected -- L]),
            exit({wrong_answer, Svc, E, RFC})
    end;

stats(?CLIENT, answer, rfc6733, L) ->
    [{{{unknown,0},recv},2},
     {{{0,257,0},recv},1},
     {{{0,257,1},send},1},
     {{{0,275,0},recv},8},
     {{{0,275,1},send},10},
     {{{unknown,0},recv,{'Result-Code',3001}},1},
     {{{unknown,0},recv,{'Result-Code',3007}},1},
     {{{0,257,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',3008}},2},
     {{{0,275,0},recv,{'Result-Code',3999}},1},
     {{{0,275,0},recv,{'Result-Code',5002}},1},
     {{{0,275,0},recv,{'Result-Code',5005}},3},
     {{{0,275,0},recv,{'Result-Code',5999}},1}]
        = L;

stats(?SERVER, answer, rfc6733, L) ->
    [{{{unknown,0},send},2},
     {{{unknown,1},recv},1},
     {{{0,257,0},send},1},
     {{{0,257,1},recv},1},
     {{{0,275,0},send},8},
     {{{0,275,1},recv},8},
     {{{unknown,0},send,{'Result-Code',3001}},1},
     {{{unknown,0},send,{'Result-Code',3007}},1},
     {{{unknown,1},recv,error},1},
     {{{0,257,0},send,{'Result-Code',2001}},1},
     {{{0,275,0},send,{'Result-Code',3008}},2},
     {{{0,275,0},send,{'Result-Code',3999}},1},
     {{{0,275,0},send,{'Result-Code',5002}},1},
     {{{0,275,0},send,{'Result-Code',5005}},3},
     {{{0,275,0},send,{'Result-Code',5999}},1},
     {{{0,275,1},recv,error},5}]
        = L;

stats(?CLIENT, answer_3xxx, rfc6733, L) ->
    [{{{unknown,0},recv},2},
     {{{0,257,0},recv},1},
     {{{0,257,1},send},1},
     {{{0,275,0},recv},8},
     {{{0,275,1},send},10},
     {{{unknown,0},recv,{'Result-Code',3001}},1},
     {{{unknown,0},recv,{'Result-Code',3007}},1},
     {{{0,257,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',3008}},2},
     {{{0,275,0},recv,{'Result-Code',3999}},1},
     {{{0,275,0},recv,{'Result-Code',5002}},1},
     {{{0,275,0},recv,{'Result-Code',5005}},2},
     {{{0,275,0},recv,{'Result-Code',5999}},1}]
        = L;

stats(?SERVER, answer_3xxx, rfc6733, L) ->
    [{{{unknown,0},send},2},
     {{{unknown,1},recv},1},
     {{{0,257,0},send},1},
     {{{0,257,1},recv},1},
     {{{0,275,0},send},8},
     {{{0,275,1},recv},8},
     {{{unknown,0},send,{'Result-Code',3001}},1},
     {{{unknown,0},send,{'Result-Code',3007}},1},
     {{{unknown,1},recv,error},1},
     {{{0,257,0},send,{'Result-Code',2001}},1},
     {{{0,275,0},send,{'Result-Code',2001}},1},
     {{{0,275,0},send,{'Result-Code',3008}},2},
     {{{0,275,0},send,{'Result-Code',3999}},1},
     {{{0,275,0},send,{'Result-Code',5002}},1},
     {{{0,275,0},send,{'Result-Code',5005}},2},
     {{{0,275,0},send,{'Result-Code',5999}},1},
     {{{0,275,1},recv,error},5}]
        = L;

stats(?CLIENT, callback, rfc3588, L) ->
    [{{{unknown,0},recv},1},
     {{{0,257,0},recv},1},
     {{{0,257,1},send},1},
     {{{0,275,0},recv},6},
     {{{0,275,1},send},10},
     {{{unknown,0},recv,{'Result-Code',3007}},1},
     {{{0,257,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',2001}},2},
     {{{0,275,0},recv,{'Result-Code',3999}},1},
     {{{0,275,0},recv,{'Result-Code',5002}},1},
     {{{0,275,0},recv,{'Result-Code',5005}},2}]
        = L;

stats(?SERVER, callback, rfc3588, L) ->
    [{{{unknown,0},send},1},
     {{{unknown,1},recv},1},
     {{{0,257,0},send},1},
     {{{0,257,1},recv},1},
     {{{0,275,0},send},6},
     {{{0,275,1},recv},8},
     {{{unknown,0},send,{'Result-Code',3007}},1},
     {{{unknown,1},recv,error},1},
     {{{0,257,0},send,{'Result-Code',2001}},1},
     {{{0,275,0},send,{'Result-Code',2001}},2},
     {{{0,275,0},send,{'Result-Code',3999}},1},
     {{{0,275,0},send,{'Result-Code',5002}},1},
     {{{0,275,0},send,{'Result-Code',5005}},2},
     {{{0,275,1},recv,error},5}]
        = L;

stats(?CLIENT, callback, rfc6733, L) ->
    [{{{unknown,0},recv},1},
     {{{0,257,0},recv},1},
     {{{0,257,1},send},1},
     {{{0,275,0},recv},8},
     {{{0,275,1},send},10},
     {{{unknown,0},recv,{'Result-Code',3007}},1},
     {{{0,257,0},recv,{'Result-Code',2001}},1},
     {{{0,275,0},recv,{'Result-Code',2001}},2},
     {{{0,275,0},recv,{'Result-Code',3999}},1},
     {{{0,275,0},recv,{'Result-Code',5002}},1},
     {{{0,275,0},recv,{'Result-Code',5005}},3},
     {{{0,275,0},recv,{'Result-Code',5999}},1}]
        = L;

stats(?SERVER, callback, rfc6733, L) ->
    [{{{unknown,0},send},1},
     {{{unknown,1},recv},1},
     {{{0,257,0},send},1},
     {{{0,257,1},recv},1},
     {{{0,275,0},send},8},
     {{{0,275,1},recv},8},
     {{{unknown,0},send,{'Result-Code',3007}},1},
     {{{unknown,1},recv,error},1},
     {{{0,257,0},send,{'Result-Code',2001}},1},
     {{{0,275,0},send,{'Result-Code',2001}},2},
     {{{0,275,0},send,{'Result-Code',3999}},1},
     {{{0,275,0},send,{'Result-Code',5002}},1},
     {{{0,275,0},send,{'Result-Code',5005}},3},
     {{{0,275,0},send,{'Result-Code',5999}},1},
     {{{0,275,1},recv,error},5}]
        = L.

%% ===========================================================================

answer(Config) ->
    RFC = proplists:get_value(rfc, Config),
    execute_testcase(?FUNCTION_NAME, RFC).

answer_3xxx(Config) ->
    RFC = proplists:get_value(rfc, Config),
    execute_testcase(?FUNCTION_NAME, RFC).

callback(Config) ->
    RFC = proplists:get_value(rfc, Config),
    execute_testcase(?FUNCTION_NAME, RFC).

%% ===========================================================================

%% send_unknown_application/2
%%
%% Send an unknown application that a callback (which shouldn't take
%% place) fails on.

%% diameter answers.
send_unknown_application(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 3007,
                                                 %% UNSUPPORTED_APPLICATION
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_unknown_command/2
%%
%% Send a unknown command that a callback discards.

%% handle_request discards the request.
send_unknown_command(callback, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    {error, timeout} = call(?FUNCTION_NAME);

%% diameter answers.
send_unknown_command(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 3001,
                                                 %% UNSUPPORTED_COMMAND
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_ok/2
%%
%% Send a correct STR that a callback answers with 5002.

%% Callback answers.
send_ok(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 5002,  %% UNKNOWN_SESSION_ID
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_invalid_hdr_bits/2
%%
%% Send a request with an incorrect E-bit that a callback ignores.

%% Callback answers.
send_invalid_hdr_bits(callback, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call(?FUNCTION_NAME);

%% diameter answers.
send_invalid_hdr_bits(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 3008, %% INVALID_HDR_BITS
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_missing_avp/2
%%
%% Send a request with a missing AVP that a callback answers.

%% diameter answers.
send_missing_avp(answer, rfc6733) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call(?FUNCTION_NAME);

%% Callback answers.
send_missing_avp(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 5005,  %% MISSING_AVP
                       'Failed-AVP' = [_],
                       'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_ignore_missing_avp/2
%%
%% Send a request with a missing AVP that a callback ignores.

%% diameter answers.
send_ignore_missing_avp(answer, rfc6733) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call(?FUNCTION_NAME);

%% Callback answers, ignores the error
send_ignore_missing_avp(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_5xxx_missing_avp/2
%%
%% Send a request with a missing AVP that a callback answers
%% with {answer_message, 5005}.

%% RFC 6733 allows 5xxx in an answer-message.
send_5xxx_missing_avp(_Errors, rfc6733) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call(?FUNCTION_NAME);

%% RFC 3588 doesn't: sending answer fails.
send_5xxx_missing_avp(_Errors, rfc3588) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    {error, timeout} = call(?FUNCTION_NAME);

%% Callback answers, ignores the error
send_5xxx_missing_avp(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_double_error/2
%%
%% Send a request with both an invalid E-bit and a missing AVP.

%% Callback answers with STA.
send_double_error(callback, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #diameter_base_STA{'Result-Code' = 5005,  %% MISSING_AVP
                       'Failed-AVP' = [_],
                       'AVP' = []}
        = call(?FUNCTION_NAME);

%% diameter answers with answer-message.
send_double_error(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 3008, %% INVALID_HDR_BITS
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_3xxx/2
%%
%% Send a request that's answered with a 3xxx result code.

%% Callback answers.
send_3xxx(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 3999,
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% send_5xxx/2
%%
%% Send a request that's answered with a 5xxx result code.

%% Callback answers but fails since 5xxx isn't allowed in an RFC 3588
%% answer-message.
send_5xxx(_Errors, rfc3588) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    {error, timeout} = call(?FUNCTION_NAME);

%% Callback answers.
send_5xxx(_Errors, _RFC) ->
    ?XL("~w -> entry", [?FUNCTION_NAME]),
    #'diameter_base_answer-message'{'Result-Code' = 5999,
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call(?FUNCTION_NAME).

%% ===========================================================================

call(Name) ->
    ?XL("call -> make diameter call with Name: ~p", [Name]),
    %% There is a "bug" in diameter, which can cause this function to return
    %% {error, timeout} even though only a fraction on the time has expired.
    %% This is because the timer has in fact *not* expired. Instead what
    %% has happened is the transport process has died and the selection
    %% of a new transport fails (I think its a race causing the pick_peer
    %% to return the same tranport process), at that error is converted to
    %% a timeout error.
    %% So, if this call returns {error, timeout} but only a fraction of the
    %% time has passed we skip instead!
    Timeout = 5000,
    T1 = ?TS(),
    case diameter:call(?CLIENT,
                       ?DICT,
                       #diameter_base_STR
                       {'Termination-Cause' = ?LOGOUT,
                        'Auth-Application-Id' = ?COMMON,
                        'Class' = [?L(Name)]},
                       [{extra, [Name]}, {timeout, Timeout}]) of
        {error, timeout} = ERROR ->
            T2 = ?TS(),
            TDiff = T2 - T1,
            if
                TDiff < 100 ->
                    exit({skip, {invalid_timeout, TDiff, Timeout}});
                true ->
                        ERROR
            end;
        R ->
            R
    end.


%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/5

pick_peer([Peer], _, ?CLIENT, _State, _Name) ->
    {ok, Peer}.

%% prepare_request/4

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, Name) ->
    {send, prepare(Pkt, Caps, Name)}.

prepare(Pkt0, Caps, send_unknown_application) ->
    Req = sta(Pkt0, Caps),
    #diameter_packet{bin = <<H:8/binary, 0:32, T/binary>>}
        = Pkt
        = diameter_codec:encode(?DICT, Pkt0#diameter_packet{msg = Req}),

    Pkt#diameter_packet{bin = <<H/binary, 23:32, T/binary>>};

prepare(Pkt0, Caps, send_unknown_command) ->
    Req = sta(Pkt0, Caps),
    #diameter_packet{bin = <<H:5/binary, 275:24, T/binary>>}
        = Pkt
        = diameter_codec:encode(?DICT, Pkt0#diameter_packet{msg = Req}),

    Pkt#diameter_packet{bin = <<H/binary, 572:24, T/binary>>};

prepare(Pkt, Caps, T)
  when T == send_ok;
       T == send_3xxx;
       T == send_5xxx ->
    sta(Pkt, Caps);

prepare(Pkt0, Caps, send_invalid_hdr_bits) ->
    Req = sta(Pkt0, Caps),
    %% Set the E-bit to force 3008.
    #diameter_packet{bin = <<H:34, 0:1, T/bitstring>>}
        = Pkt
        = diameter_codec:encode(?DICT, Pkt0#diameter_packet{msg = Req}),
    Pkt#diameter_packet{bin = <<H:34, 1:1, T/bitstring>>};

prepare(Pkt0, Caps, send_double_error) ->
    dehost(prepare(Pkt0, Caps, send_invalid_hdr_bits));

prepare(Pkt, Caps, T)
  when T == send_missing_avp;
       T == send_ignore_missing_avp;
       T == send_5xxx_missing_avp ->
    Req = sta(Pkt, Caps),
    dehost(diameter_codec:encode(?DICT, Pkt#diameter_packet{msg = Req})).

sta(Pkt, Caps) ->
    #diameter_packet{msg = Req}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    Req#diameter_base_STR{'Session-Id' = diameter:session_id(OH),
                          'Origin-Host' = OH,
                          'Origin-Realm' = OR,
                          'Destination-Realm' = DR}.

%% Strip Origin-Host.
dehost(#diameter_packet{bin = Bin} = Pkt) ->
    <<V, Len:24, H:16/binary, T0/binary>>
        = Bin,
    {SessionId, T1} = split_avp(T0),
    {OriginHost, T} = split_avp(T1),
    Delta = size(OriginHost),
    Pkt#diameter_packet{bin = <<V, (Len - Delta):24, H/binary,
                                SessionId/binary,
                                T/binary>>}.

%% handle_answer/5

handle_answer(Pkt, _Req, ?CLIENT, _Peer, _Name) ->
    Pkt#diameter_packet.msg.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer, _Name) ->
    {error, Reason}.

split_avp(<<_:5/binary, Len:24, _/binary>> = Bin) ->
    L = pad(Len),
    <<Avp:L/binary, T/binary>> = Bin,
    {Avp, T}.

pad(N)
  when 0 == N rem 4 ->
    N;
pad(N) ->
    N - (N rem 4) + 4.

%% handle_request/3

handle_request(#diameter_packet{header = #diameter_header{application_id = 0},
                                msg = Msg},
               ?SERVER,
               {_, Caps}) ->
    request(Msg, Caps).

request(undefined, _) ->  %% unknown command
    discard;

request(#diameter_base_STR{'Class' = [Name]} = Req, Caps) ->
    request(?A(Name), Req, Caps).

request(send_ok, Req, Caps) ->
    {reply, #diameter_packet{msg = answer(Req, Caps),
                             errors = [5002]}};  %% UNKNOWN_SESSION_ID

request(send_3xxx, _Req, _Caps) ->
    {answer_message, 3999};

request(send_5xxx, _Req, _Caps) ->
    {answer_message, 5999};

request(send_invalid_hdr_bits, Req, Caps) ->
    %% Default errors field but a non-answer-message and only 3xxx
    %% errors detected means diameter sets neither Result-Code nor
    %% Failed-AVP.
    {reply, #diameter_packet{msg = answer(Req, Caps)}};

request(T, Req, Caps)
  when T == send_double_error;
       T == send_missing_avp ->
    {reply, answer(Req, Caps)};

request(send_ignore_missing_avp, Req, Caps) ->
    {reply, #diameter_packet{msg = answer(Req, Caps),
                             errors = false}};  %% ignore errors

request(send_5xxx_missing_avp, _Req, _Caps) ->
    {answer_message, 5005}.  %% MISSING_AVP

answer(Req, Caps) ->
    #diameter_base_STR{'Session-Id' = SId}
        = Req,
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_STA{'Session-Id' = SId,
                       'Origin-Host' = OH,
                       'Origin-Realm' = OR,
                       'Result-Code' = 2001}.  %% SUCCESS
