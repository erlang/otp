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
%% Tests of application_opt() request_errors. There's some overlap
%% between this suite and the traffic suite but latter exercises more
%% config.
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
         send_unknown_application/1,
         send_unknown_command/1,
         send_ok/1,
         send_invalid_hdr_bits/1,
         send_missing_avp/1,
         send_ignore_missing_avp/1,
         send_5xxx_missing_avp/1,
         send_double_error/1,
         send_3xxx/1,
         send_5xxx/1,
         counters/1,
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
-include("diameter_gen_base_rfc6733.hrl").
%% Use the fact that STR/STA is identical in RFC's 3588 and 6733.

%% ===========================================================================

-define(util, diameter_util).
-define(testcase(), proplists:get_value(testcase, get(?MODULE))).
-define(group(Config), begin
                           put(?MODULE, Config),
                           ?util:name(proplists:get_value(group, Config))
                       end).

-define(L, atom_to_list).
-define(A, list_to_atom).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).

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

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, ?util:name([E,D])} || E <- ?ERRORS, D <- ?RFCS].

groups() ->
    Tc = tc(),
    [{?util:name([E,D]), [], [start] ++ Tc ++ [counters, stop]}
     || E <- ?ERRORS, D <- ?RFCS].

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

init_per_group(Group, Config) ->
    [{group, Group} | Config].

end_per_group(_, _) ->
    ok.

init_per_testcase(Name, Config) ->
    [{testcase, Name} | Config].

end_per_testcase(_, _) ->
    ok.

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

%% ===========================================================================

%% start/1

start(Config) ->
    Group = proplists:get_value(group, Config),
    [Errors, RFC] = ?util:name(Group),
    ok = diameter:start_service(?SERVER, ?SERVICE(?L(Group),
                                                  Errors,
                                                  RFC)),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT,
                                                  callback,
                                                  rfc6733)),
    LRef = ?util:listen(?SERVER, tcp),
    ?util:connect(?CLIENT, tcp, LRef).

%% stop/1

stop(_Config) ->
    ok = diameter:remove_transport(?CLIENT, true),
    ok = diameter:remove_transport(?SERVER, true),
    ok = diameter:stop_service(?SERVER),
    ok = diameter:stop_service(?CLIENT).

%% counters/1
%%
%% Check that counters are as expected.

counters(Config) ->
    Group = proplists:get_value(group, Config),
    [_Errors, _Rfc] = G = ?util:name(Group),
    [] = ?util:run([[fun counters/3, K, S, G]
                    || K <- [statistics, transport, connections],
                       S <- [?CLIENT, ?SERVER]]).

counters(Key, Svc, Group) ->
    counters(Key, Svc, Group, [_|_] = diameter:service_info(Svc, Key)).

counters(statistics, Svc, [Errors, Rfc], L) ->
    [{P, Stats}] = L,
    true = is_pid(P),
    stats(Svc, Errors, Rfc, lists:sort(Stats));

counters(_, _, _, _) ->
    todo.

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

stats(?SERVER, E, rfc3588, L)
  when E == answer;
       E == answer_3xxx ->
    [{{{unknown,0},send},2},
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
     {{{0,275,1},recv,error},5}]
        = L;

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

%% send_unknown_application/1
%%
%% Send an unknown application that a callback (which shouldn't take
%% place) fails on.

%% diameter answers.
send_unknown_application([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 3007,
                                                 %% UNSUPPORTED_APPLICATION
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_unknown_application(Config) ->
    send_unknown_application(?group(Config)).

%% send_unknown_command/1
%%
%% Send a unknown command that a callback discards.

%% handle_request discards the request.
send_unknown_command([callback, _]) ->
    {error, timeout} = call();

%% diameter answers.
send_unknown_command([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 3001,
                                                 %% UNSUPPORTED_COMMAND
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_unknown_command(Config) ->
    send_unknown_command(?group(Config)).

%% send_ok/1
%%
%% Send a correct STR that a callback answers with 5002.

%% Callback answers.
send_ok([_,_]) ->
    #diameter_base_STA{'Result-Code' = 5002,  %% UNKNOWN_SESSION_ID
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call();

send_ok(Config) ->
    send_ok(?group(Config)).

%% send_invalid_hdr_bits/1
%%
%% Send a request with an incorrect E-bit that a callback ignores.

%% Callback answers.
send_invalid_hdr_bits([callback, _]) ->
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call();

%% diameter answers.
send_invalid_hdr_bits([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 3008, %% INVALID_HDR_BITS
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_invalid_hdr_bits(Config) ->
    send_invalid_hdr_bits(?group(Config)).

%% send_missing_avp/1
%%
%% Send a request with a missing AVP that a callback answers.

%% diameter answers.
send_missing_avp([answer, rfc6733]) ->
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call();

%% Callback answers.
send_missing_avp([_,_]) ->
    #diameter_base_STA{'Result-Code' = 5005,  %% MISSING_AVP
                       'Failed-AVP' = [_],
                       'AVP' = []}
        = call();

send_missing_avp(Config) ->
    send_missing_avp(?group(Config)).

%% send_ignore_missing_avp/1
%%
%% Send a request with a missing AVP that a callback ignores.

%% diameter answers.
send_ignore_missing_avp([answer, rfc6733]) ->
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call();

%% Callback answers, ignores the error
send_ignore_missing_avp([_,_]) ->
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call();

send_ignore_missing_avp(Config) ->
    send_ignore_missing_avp(?group(Config)).

%% send_5xxx_missing_avp/1
%%
%% Send a request with a missing AVP that a callback answers
%% with {answer_message, 5005}.

%% RFC 6733 allows 5xxx in an answer-message.
send_5xxx_missing_avp([_, rfc6733]) ->
    #'diameter_base_answer-message'{'Result-Code' = 5005,  %% MISSING_AVP
                                    'Failed-AVP' = [_],
                                    'AVP' = []}
        = call();

%% RFC 3588 doesn't: sending answer fails.
send_5xxx_missing_avp([_, rfc3588]) ->
    {error, timeout} = call();

%% Callback answers, ignores the error
send_5xxx_missing_avp([_,_]) ->
    #diameter_base_STA{'Result-Code' = 2001,  %% SUCCESS
                       'Failed-AVP' = [],
                       'AVP' = []}
        = call();

send_5xxx_missing_avp(Config) ->
    send_5xxx_missing_avp(?group(Config)).

%% send_double_error/1
%%
%% Send a request with both an invalid E-bit and a missing AVP.

%% Callback answers with STA.
send_double_error([callback, _]) ->
    #diameter_base_STA{'Result-Code' = 5005,  %% MISSING_AVP
                       'Failed-AVP' = [_],
                       'AVP' = []}
        = call();

%% diameter answers with answer-message.
send_double_error([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 3008, %% INVALID_HDR_BITS
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_double_error(Config) ->
    send_double_error(?group(Config)).

%% send_3xxx/1
%%
%% Send a request that's answered with a 3xxx result code.

%% Callback answers.
send_3xxx([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 3999,
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_3xxx(Config) ->
    send_3xxx(?group(Config)).

%% send_5xxx/1
%%
%% Send a request that's answered with a 5xxx result code.

%% Callback answers but fails since 5xxx isn't allowed in an RFC 3588
%% answer-message.
send_5xxx([_, rfc3588]) ->
    {error, timeout} = call();

%% Callback answers.
send_5xxx([_,_]) ->
    #'diameter_base_answer-message'{'Result-Code' = 5999,
                                    'Failed-AVP' = [],
                                    'AVP' = []}
        = call();

send_5xxx(Config) ->
    send_5xxx(?group(Config)).

%% ===========================================================================

call() ->
    Name = ?testcase(),
    diameter:call(?CLIENT,
                  ?DICT,
                  #diameter_base_STR
                  {'Termination-Cause' = ?LOGOUT,
                   'Auth-Application-Id' = ?COMMON,
                   'Class' = [?L(Name)]},
                  [{extra, [Name]}]).

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
