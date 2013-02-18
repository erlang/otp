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
         send_unknown_command/1,
         send_ok_override/1,
         send_invalid_avp_bits/1,
         send_double_error/1,
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

-define(SUCCESS,              2001).
-define(UNSUPPORTED_COMMAND,  3001).
-define(INVALID_AVP_BITS,     3009).
-define(UNKNOWN_SESSION_ID,   5002).
-define(MISSING_AVP,          5005).

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').

-define(GROUPS, [answer_3xxx, callback]).
-define(L, atom_to_list).
-define(A, list_to_atom).
-define(v, proplists:get_value).

-define(testcase(Config), put({?MODULE, testcase}, ?v(testcase, Config))).
-define(testcase(), get({?MODULE, testcase})).
-define(group(Config), ?v(group, Config)).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, G} || G <- ?GROUPS].

groups() ->
    Tc = tc(),
    [{G, [], [start] ++ Tc ++ [stop]} || G <- ?GROUPS].

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

origin(answer_3xxx)   -> 0;
origin(callback) -> 1;

origin(0) -> answer_3xxx;
origin(1) -> callback.

tc() ->
    [send_unknown_command,
     send_ok_override,
     send_invalid_avp_bits,
     send_double_error].

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

%% send_unknown_command/1
%%
%% Send a unknown command and expect a different result depending on
%% whether or not the server gets a handle_request callback.

%% Server handle_request discards the request.
send_unknown_command(callback) ->
    {error, timeout} = call();

%% No handle_request, diameter answers.
send_unknown_command(answer_3xxx) ->
    #'diameter_base_answer-message'{'Result-Code' = ?UNSUPPORTED_COMMAND}
        = call();

send_unknown_command(Config) ->
    ?testcase(Config),
    send_unknown_command(?group(Config)).

%% send_ok_override/1
%%
%% Send a correct STA and expect the same answer from handle_request
%% in both cases.

send_ok_override(A)
  when is_atom(A) ->
    #diameter_base_STA{'Result-Code' = ?UNKNOWN_SESSION_ID}
        = call();

send_ok_override(Config) ->
    ?testcase(Config),
    send_ok_override(?group(Config)).

%% send_invalid_avp_bits/1
%%
%% Send a request with an incorrect length on the optional
%% Origin-State-Id and expect a callback to ignore the error.

send_invalid_avp_bits(callback) ->
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Failed-AVP' = []}
        = call();

send_invalid_avp_bits(answer_3xxx) ->
    #'diameter_base_answer-message'{'Result-Code' = ?INVALID_AVP_BITS,
                                    'Failed-AVP' = []}
        = call();

send_invalid_avp_bits(Config) ->
    ?testcase(Config),
    send_invalid_avp_bits(?group(Config)).

%% send_double_error/1
%%
%% Send a request with both an incorrect length on the optional
%% Origin-State-Id and a missing AVP and see that it's answered
%% differently.

%% diameter answers with the 3xxx error.
send_double_error(answer_3xxx) ->
    #'diameter_base_answer-message'{'Result-Code' = ?INVALID_AVP_BITS,
                                    'Failed-AVP' = [_]}
        = call();

%% handle_request answers with STA and diameter resets Result-Code.
send_double_error(callback) ->
    #diameter_base_STA{'Result-Code' = ?MISSING_AVP,
                       'Failed-AVP' = [_]}
        = call();

send_double_error(Config) ->
    ?testcase(Config),
    send_double_error(?group(Config)).

%% ===========================================================================

call() ->
    Name = ?testcase(),
    diameter:call(?CLIENT,
                  ?DICT,
                  #diameter_base_STR
                  {'Termination-Cause' = ?LOGOUT,
                   'Auth-Application-Id' = ?DIAMETER_APP_ID_COMMON,
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

prepare(Pkt0, Caps, send_unknown_command) ->
    #diameter_packet{msg = Req0}
        = Pkt0,
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

    Pkt#diameter_packet{bin = <<H/binary, 572:24, T/binary>>};

prepare(Pkt, Caps, send_ok_override) ->
    #diameter_packet{msg = Req}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    Req#diameter_base_STR{'Session-Id' = diameter:session_id(OH),
                          'Origin-Host' = OH,
                          'Origin-Realm' = OR,
                          'Destination-Realm' = DR};

prepare(Pkt, Caps, send_invalid_avp_bits) ->
    #diameter_packet{msg = Req0}
        = Pkt,
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    %% Append an Origin-State-Id with an incorrect AVP Length in order
    %% to force 3009.
    Req = Req0#diameter_base_STR{'Session-Id' = diameter:session_id(OH),
                                 'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Destination-Realm' = DR,
                                 'Origin-State-Id' = [7]},
    #diameter_packet{bin = Bin}
        = diameter_codec:encode(?DICT, Pkt#diameter_packet{msg = Req}),
    Offset = size(Bin) - 12 + 5,
    <<H:Offset/binary, Len:24, T/binary>> = Bin,
    Pkt#diameter_packet{bin = <<H/binary, (Len + 2):24, T/binary>>};

prepare(Pkt0, Caps, send_double_error) ->
    #diameter_packet{bin = Bin}
        = Pkt
        = prepare(Pkt0, Caps, send_invalid_avp_bits),
    %% Keep Session-Id but remove Origin-Host.
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

%% send_unknown_command
handle_request(#diameter_packet{msg = undefined}, ?SERVER, _) ->
    discard;

handle_request(#diameter_packet{msg = Req}, ?SERVER, {_, Caps}) ->
    #diameter_base_STR{'Class' = [Name]}
        = Req,
    {reply, request(?A(Name), Req, Caps)}.

request(send_ok_override, Req, Caps) ->
    #diameter_packet{msg = answer(Req, Caps),
                     errors = [?UNKNOWN_SESSION_ID]};  %% override

request(send_invalid_avp_bits, Req, Caps) ->
    #diameter_base_STR{'Origin-State-Id' = []}
        = Req,
    %% Default errors field but a non-answer-message and only 3xxx
    %% errors detected means diameter sets neither Result-Code nor
    %% Failed-AVP.
    #diameter_packet{msg = answer(Req, Caps)};

request(send_double_error, Req, Caps) ->
    answer(Req, Caps).

answer(Req, Caps) ->
    #diameter_base_STR{'Session-Id' = SId}
        = Req,
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_STA{'Session-Id' = SId,
                       'Origin-Host' = OH,
                       'Origin-Realm' = OR,
                       'Result-Code' = ?SUCCESS}.
