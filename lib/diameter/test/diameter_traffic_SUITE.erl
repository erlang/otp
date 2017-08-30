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
%% Tests of traffic between two Diameter nodes, one client, one server.
%%

-module(diameter_traffic_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         start_services/1,
         add_transports/1,
         result_codes/1,
         send_ok/1,
         send_nok/1,
         send_eval/1,
         send_bad_answer/1,
         send_protocol_error/1,
         send_experimental_result/1,
         send_arbitrary/1,
         send_unknown/1,
         send_unknown_short/1,
         send_unknown_mandatory/1,
         send_unknown_short_mandatory/1,
         send_noreply/1,
         send_grouped_error/1,
         send_unsupported/1,
         send_unsupported_app/1,
         send_error_bit/1,
         send_unsupported_version/1,
         send_long_avp_length/1,
         send_short_avp_length/1,
         send_zero_avp_length/1,
         send_invalid_avp_length/1,
         send_invalid_reject/1,
         send_unexpected_mandatory_decode/1,
         send_unexpected_mandatory/1,
         send_long/1,
         send_maxlen/1,
         send_nopeer/1,
         send_noapp/1,
         send_discard/1,
         send_any_1/1,
         send_any_2/1,
         send_all_1/1,
         send_all_2/1,
         send_timeout/1,
         send_error/1,
         send_detach/1,
         send_encode_error/1,
         send_destination_1/1,
         send_destination_2/1,
         send_destination_3/1,
         send_destination_4/1,
         send_destination_5/1,
         send_destination_6/1,
         send_bad_option_1/1,
         send_bad_option_2/1,
         send_bad_filter_1/1,
         send_bad_filter_2/1,
         send_bad_filter_3/1,
         send_bad_filter_4/1,
         send_multiple_filters_1/1,
         send_multiple_filters_2/1,
         send_multiple_filters_3/1,
         send_anything/1,
         outstanding/1,
         remove_transports/1,
         empty/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/6, pick_peer/7,
         prepare_request/5, prepare_request/6,
         prepare_retransmit/5,
         handle_answer/6, handle_answer/7,
         handle_error/6,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").
-include("diameter_gen_base_accounting.hrl").
%% The listening transports use RFC 3588 dictionaries, the client
%% transports use either 3588 or 6733. (So can't use the record
%% definitions in the latter case.)

%% ===========================================================================

-define(util, diameter_util).

-define(A, list_to_atom).
-define(L, atom_to_list).

%% Don't use is_record/2 since dictionary hrl's aren't included.
%% (Since they define conflicting reqcords with the same names.)
-define(is_record(Rec, Name), (Name == element(1, Rec))).

-define(ADDR, {127,0,0,1}).

-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).

-define(EXTRA, an_extra_argument).

%% Sequence mask for End-to-End and Hop-by-Hop identifiers.
-define(CLIENT_MASK, {1,26}).  %% 1 in top 6 bits

%% How to construct messages, as record or list.
-define(ENCODINGS, [list, record]).

%% How to send answers, in a diameter_packet or not.
-define(CONTAINERS, [pkt, msg]).

%% Which common dictionary to use in the clients.
-define(RFCS, [rfc3588, rfc6733]).

%% Whether to decode stringish Diameter types to strings, or leave
%% them as binary.
-define(STRING_DECODES, [true, false]).

%% Which transport protocol to use.
-define(TRANSPORTS, [tcp, sctp]).

-record(group,
        {transport,
         client_service,
         client_encoding,
         client_dict0,
         client_strings,
         server_service,
         server_encoding,
         server_container,
         server_strings}).

%% Not really what we should be setting unless the message is sent in
%% the common application but diameter doesn't care.
-define(APP_ID, ?DIAMETER_APP_ID_COMMON).

%% An Application-ID the server doesn't support.
-define(BAD_APP, 42).

%% A common match when receiving answers in a client.
-define(answer_message(SessionId, ResultCode),
        ['answer-message',
         {'Session-Id', SessionId},
         {'Origin-Host', _},
         {'Origin-Realm', _},
         {'Result-Code', ResultCode}
         | _]).
-define(answer_message(ResultCode),
        ?answer_message(_, ResultCode)).

%% Config for diameter:start_service/2.
-define(SERVICE(Name, Decode),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {'Acct-Application-Id', [?DIAMETER_APP_ID_ACCOUNTING]},
         {restrict_connections, false},
         {string_decode, Decode},
         {incoming_maxlen, 1 bsl 21},
         {spawn_opt, [{min_heap_size, 5000}]}
         | [{application, [{dictionary, D},
                           {module, ?MODULE},
                           {answer_errors, callback}]}
            || D <- [diameter_gen_base_rfc3588,
                     diameter_gen_base_accounting,
                     diameter_gen_base_rfc6733,
                     diameter_gen_acct_rfc6733]]]).

-define(SUCCESS,
        ?'DIAMETER_BASE_RESULT-CODE_SUCCESS').
-define(COMMAND_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_COMMAND_UNSUPPORTED').
-define(TOO_BUSY,
        ?'DIAMETER_BASE_RESULT-CODE_TOO_BUSY').
-define(APPLICATION_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_APPLICATION_UNSUPPORTED').
-define(INVALID_HDR_BITS,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_HDR_BITS').
-define(INVALID_AVP_BITS,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS').
-define(AVP_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED').
-define(UNSUPPORTED_VERSION,
        ?'DIAMETER_BASE_RESULT-CODE_UNSUPPORTED_VERSION').
-define(REALM_NOT_SERVED,
        ?'DIAMETER_BASE_RESULT-CODE_REALM_NOT_SERVED').
-define(UNABLE_TO_DELIVER,
        ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_DELIVER').
-define(INVALID_AVP_LENGTH,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH').

-define(EVENT_RECORD,
        ?'DIAMETER_BASE_ACCOUNTING-RECORD-TYPE_EVENT_RECORD').
-define(AUTHORIZE_ONLY,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').
-define(AUTHORIZE_AUTHENTICATE,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE').

-define(LOGOUT,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(BAD_ANSWER,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_BAD_ANSWER').
-define(USER_MOVED,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_USER_MOVED').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [start, result_codes, {group, traffic}, outstanding, empty, stop].

groups() ->
    Ts = tc(),
    Sctp = ?util:have_sctp(),
    [{?util:name([R,D,A,C]), [parallel], Ts} || R <- ?ENCODINGS,
                                                D <- ?RFCS,
                                                A <- ?ENCODINGS,
                                                C <- ?CONTAINERS]
        ++
        [{?util:name([T,R,D,A,C,SD,CD]),
          [],
          [start_services,
           add_transports,
           result_codes,
           {group, ?util:name([R,D,A,C])},
           remove_transports,
           stop_services]}
         || T <- ?TRANSPORTS,
            T /= sctp orelse Sctp,
            R <- ?ENCODINGS,
            D <- ?RFCS,
            A <- ?ENCODINGS,
            C <- ?CONTAINERS,
            SD <- ?STRING_DECODES,
            CD <- ?STRING_DECODES]
        ++
        [{traffic, [], [{group, ?util:name([T,R,D,A,C,SD,CD])}
                        || T <- ?TRANSPORTS,
                           T /= sctp orelse Sctp,
                           R <- ?ENCODINGS,
                           D <- ?RFCS,
                           A <- ?ENCODINGS,
                           C <- ?CONTAINERS,
                           SD <- ?STRING_DECODES,
                           CD <- ?STRING_DECODES]}].

init_per_group(Name, Config) ->
    case ?util:name(Name) of
        [T,R,D,A,C,SD,CD] ->
            G = #group{transport = T,
                       client_service = [$C|?util:unique_string()],
                       client_encoding = R,
                       client_dict0 = dict0(D),
                       client_strings = CD,
                       server_service = [$S|?util:unique_string()],
                       server_encoding = A,
                       server_container = C,
                       server_strings = SD},
            [{group, G} | Config];
        _ ->
            Config
    end.

end_per_group(_, _) ->
    ok.

%% Skip testcases that can reasonably fail under SCTP.
init_per_testcase(Name, Config) ->
    case [skip || #group{transport = sctp}
                      <- [proplists:get_value(group, Config)],
                  send_maxlen == Name
                      orelse send_long == Name]
    of
        [skip] ->
            {skip, sctp};
        [] ->
            [{testcase, Name} | Config]
    end.

end_per_testcase(_, _) ->
    ok.

%% Testcases to run when services are started and connections
%% established.
tc() ->
    [send_ok,
     send_nok,
     send_eval,
     send_bad_answer,
     send_protocol_error,
     send_experimental_result,
     send_arbitrary,
     send_unknown,
     send_unknown_short,
     send_unknown_mandatory,
     send_unknown_short_mandatory,
     send_noreply,
     send_grouped_error,
     send_unsupported,
     send_unsupported_app,
     send_error_bit,
     send_unsupported_version,
     send_long_avp_length,
     send_short_avp_length,
     send_zero_avp_length,
     send_invalid_avp_length,
     send_invalid_reject,
     send_unexpected_mandatory_decode,
     send_unexpected_mandatory,
     send_long,
     send_maxlen,
     send_nopeer,
     send_noapp,
     send_discard,
     send_any_1,
     send_any_2,
     send_all_1,
     send_all_2,
     send_timeout,
     send_error,
     send_detach,
     send_encode_error,
     send_destination_1,
     send_destination_2,
     send_destination_3,
     send_destination_4,
     send_destination_5,
     send_destination_6,
     send_bad_option_1,
     send_bad_option_2,
     send_bad_filter_1,
     send_bad_filter_2,
     send_bad_filter_3,
     send_bad_filter_4,
     send_multiple_filters_1,
     send_multiple_filters_2,
     send_multiple_filters_3,
     send_anything].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(Config) ->
    #group{client_service = CN,
           client_strings = CD,
           server_service = SN,
           server_strings = SD}
        = group(Config),
    ok = diameter:start_service(SN, ?SERVICE(SN, SD)),
    ok = diameter:start_service(CN, [{sequence, ?CLIENT_MASK}
                                     | ?SERVICE(CN, CD)]).

add_transports(Config) ->
    #group{transport = T,
           client_service = CN,
           server_service = SN}
        = group(Config), 
    LRef = ?util:listen(SN,
                        T,
                        [{capabilities_cb, fun capx/2},
                         {pool_size, 8},
                         {spawn_opt, [{min_heap_size, 8096}]},
                         {applications, apps(rfc3588)}]),
    Cs = [?util:connect(CN,
                        T,
                        LRef,
                        [{id, Id},
                         {capabilities, [{'Origin-State-Id', origin(Id)}]},
                         {applications, apps(D)}])
          || A <- ?ENCODINGS,
             C <- ?CONTAINERS,
             D <- ?RFCS,
             Id <- [{A,C}]],
    %% The server uses the client's Origin-State-Id to decide how to
    %% answer.
    ?util:write_priv(Config, "transport", [LRef | Cs]).

apps(D0) ->
    D = dict0(D0),
    [acct(D), D].

%% Ensure there are no outstanding requests in request table.
outstanding(_Config) ->
    [] = [T || T <- ets:tab2list(diameter_request),
               is_atom(element(1,T))].

remove_transports(Config) ->
    #group{client_service = CN,
           server_service = SN}
        = group(Config),
    [LRef | Cs] = ?util:read_priv(Config, "transport"),
    try
        [] = [T || C <- Cs, T <- [?util:disconnect(CN, C, SN, LRef)], T /= ok]
    after
        ok = diameter:remove_transport(SN, LRef)
    end.

stop_services(Config) ->
    #group{client_service = CN,
           server_service = SN}
        = group(Config),
    ok = diameter:stop_service(CN),
    ok = diameter:stop_service(SN).

%% Ensure even transports have been removed from request table.
empty(_Config) ->
    [] = ets:tab2list(diameter_request).

stop(_Config) ->
    ok = diameter:stop().

capx(_, #diameter_caps{origin_host = {OH,DH}}) ->
    io:format("connection: ~p -> ~p~n", [DH,OH]),
    ok.

%% ===========================================================================

%% Ensure that result codes have the expected values.
result_codes(_Config) ->
    {2001, 3001, 3002, 3003, 3004, 3007, 3008, 3009, 5001, 5011, 5014}
        = {?SUCCESS,
           ?COMMAND_UNSUPPORTED,
           ?UNABLE_TO_DELIVER,
           ?REALM_NOT_SERVED,
           ?TOO_BUSY,
           ?APPLICATION_UNSUPPORTED,
           ?INVALID_HDR_BITS,
           ?INVALID_AVP_BITS,
           ?AVP_UNSUPPORTED,
           ?UNSUPPORTED_VERSION,
           ?INVALID_AVP_LENGTH}.

%% Send an ACR and expect success.
send_ok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 1}],

    ['ACA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send an accounting ACR that the server answers badly to.
send_nok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 0}],

    ?answer_message(?INVALID_AVP_BITS)
        = call(Config, Req).

%% Send an ACR and expect success.
send_eval(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 3}],

    ['ACA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send an accounting ACR that the server tries to answer with an
%% inappropriate header. That the error is detected is coded in
%% handle_answer.
send_bad_answer(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 2}],
    ?answer_message(?SUCCESS)
        = call(Config, Req).

%% Send an ACR that the server callback answers explicitly with a
%% protocol error.
send_protocol_error(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 4}],

    ?answer_message(?TOO_BUSY)
        = call(Config, Req).

%% Send a 3xxx Experimental-Result in an answer not setting the E-bit
%% and missing a Result-Code.
send_experimental_result(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 5}],
    ['ACA', {'Session-Id', _} | _]
        = call(Config, Req).

%% Send an ASR with an arbitrary non-mandatory AVP and expect success
%% and the same AVP in the reply.
send_arbitrary(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{name = 'Product-Name',
                                         value = "XXX"}]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | Avps]
        = call(Config, Req),
    {'AVP', [#diameter_avp{name = 'Product-Name',
                           value = V}]}
        = lists:last(Avps),
    "XXX" = string(V, Config).

%% Send an unknown AVP (to some client) and check that it comes back.
send_unknown(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = false,
                                         data = <<17>>}]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | Avps]
        = call(Config, Req),
    {'AVP', [#diameter_avp{code = 999,
                           is_mandatory = false,
                           data = <<17>>}]}
        = lists:last(Avps).

%% Ditto, and point the AVP length past the end of the message. Expect
%% 5014.
send_unknown_short(Config) ->
    send_unknown_short(Config, false, ?INVALID_AVP_LENGTH).

send_unknown_short(Config, M, RC) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = M,
                                         data = <<17>>}]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', RC} | Avps]
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = As}]
        = proplists:get_value('Failed-AVP', Avps),
    [#diameter_avp{code = 999,
                   is_mandatory = M,
                   data = <<17, _/binary>>}]  %% extra bits from padding
        = As.

%% Ditto but set the M flag.
send_unknown_mandatory(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = true,
                                         data = <<17>>}]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', ?AVP_UNSUPPORTED} | Avps]
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = As}]
        = proplists:get_value('Failed-AVP', Avps),
    [#diameter_avp{code = 999,
                   is_mandatory = true,
                   data = <<17>>}]
        = As.

%% Ditto, and point the AVP length past the end of the message. Expect
%% 5014 instead of 5001.
send_unknown_short_mandatory(Config) ->
    send_unknown_short(Config, true, ?INVALID_AVP_LENGTH).

%% Send an ASR containing an unexpected mandatory Session-Timeout.
%% Expect 5001, and check that the value in Failed-AVP was decoded.
send_unexpected_mandatory_decode(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 27,  %% Session-Timeout
                                         is_mandatory = true,
                                         data = <<12:32>>}]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', ?AVP_UNSUPPORTED} | Avps]
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = As}]
        = proplists:get_value('Failed-AVP', Avps),
    [#diameter_avp{code = 27,
                   is_mandatory = true,
                   value = 12,
                   data = <<12:32>>}]
        = As.

%% Send an containing a faulty Grouped AVP (empty Proxy-Host in
%% Proxy-Info) and expect that only the faulty AVP is sent in
%% Failed-AVP. The encoded values of Proxy-Host and Proxy-State are
%% swapped in prepare_request since an empty Proxy-Host is an encode
%% error.
send_grouped_error(Config) ->
    Req = ['ASR', {'Proxy-Info', [[{'Proxy-Host', "abcd"},
                                   {'Proxy-State', ""}]]}],
    ['ASA', {'Session-Id', _}, {'Result-Code', ?INVALID_AVP_LENGTH} | Avps]
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = As}]
        = proplists:get_value('Failed-AVP', Avps),
    [#diameter_avp{name = 'Proxy-Info',
                   value = #'diameter_base_Proxy-Info'
                            {'Proxy-Host' = Empty,
                             'Proxy-State' = undefined}}]
        = As,
    <<0>> = iolist_to_binary(Empty).

%% Send an STR that the server ignores.
send_noreply(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    {timeout, _} = call(Config, Req).

%% Send an unsupported command and expect 3001.
send_unsupported(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?COMMAND_UNSUPPORTED)
        = call(Config, Req).

%% Send an unsupported application and expect 3007.
send_unsupported_app(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?APPLICATION_UNSUPPORTED)
        = call(Config, Req).

%% Send a request with the E bit set and expect 3008.
send_error_bit(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?INVALID_HDR_BITS)
        = call(Config, Req).

%% Send a bad version and check that we get 5011.
send_unsupported_version(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    ['STA', {'Session-Id', _}, {'Result-Code', ?UNSUPPORTED_VERSION} | _]
        = call(Config, Req).

%% Send a request containing an AVP length > data size.
send_long_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP length < data size.
send_short_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP whose advertised length is < 8.
send_zero_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP length that doesn't match the
%% AVP's type.
send_invalid_avp_length(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],

    ['STA', {'Session-Id', _},
            {'Result-Code', ?INVALID_AVP_LENGTH},
            {'Origin-Host', _},
            {'Origin-Realm', _},
            {'User-Name', _},
            {'Class', _},
            {'Error-Message', _},
            {'Error-Reporting-Host', _},
            {'Failed-AVP', [#'diameter_base_Failed-AVP'{'AVP' = [_]}]}
          | _]
        = call(Config, Req).

%% Send a request containing 5xxx errors that the server rejects with
%% 3xxx.
send_invalid_reject(Config) ->
    Req = ['STR', {'Termination-Cause', ?USER_MOVED}],

    ?answer_message(?TOO_BUSY)
        = call(Config, Req).

%% Send an STR containing a known AVP, but one that's not expected and
%% that sets the M-bit.
send_unexpected_mandatory(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],

    ['STA', {'Session-Id', _}, {'Result-Code', ?AVP_UNSUPPORTED} | _]
        = call(Config, Req).

%% Send something long that will be fragmented by TCP.
send_long(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [lists:duplicate(1 bsl 20, $X)]}],
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send something longer than the configure incoming_maxlen.
send_maxlen(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [lists:duplicate(1 bsl 21, $X)]}],
    {timeout, _} = call(Config, Req).

%% Send something for which pick_peer finds no suitable peer.
send_nopeer(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{extra, [?EXTRA]}]).

%% Send something on an unconfigured application.
send_noapp(Config) ->
    #group{client_service = CN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = diameter:call(CN, unknown_alias, Req).

%% Send something that's discarded by prepare_request.
send_discard(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, unprepared} = call(Config, Req).

%% Send with a disjunctive filter.
send_any_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{filter, {any, []}}]).
send_any_2(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, "unknown.org")]}],
    ?answer_message(?UNABLE_TO_DELIVER)
        = call(Config, Req, [{filter, {first, [{all, [host, realm]},
                                               realm]}}]).

%% Send with a conjunctive filter.
send_all_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Realm = lists:foldr(fun(C,A) -> [C,A] end, [], ?REALM),
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [{host, any},
                                             {realm, Realm}]}}]).
send_all_2(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Timeout before the server manages an answer.
send_timeout(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_ONLY}],
    {timeout, _} = call(Config, Req, [{timeout, 1000}]).

%% Explicitly answer with an answer-message and ensure that we
%% received the Session-Id.
send_error(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_AUTHENTICATE}],
    ?answer_message([_], ?TOO_BUSY)
        = call(Config, Req).

%% Send a request with the detached option and receive it as a message
%% from handle_answer instead.
send_detach(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Ref = make_ref(),
    ok = call(Config, Req, [{extra, [{self(), Ref}]}, detach]),
    Ans = receive {Ref, T} -> T end,
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = Ans.

%% Send a request which can't be encoded and expect {error, encode}.
send_encode_error(Config) ->
    {error, encode} = call(Config, ['STR']).  %% No Termination-Cause

%% Send with filtering and expect success.
send_destination_1(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, ?REALM)]}],
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send with filtering on and expect failure when specifying an
%% unknown host or realm.
send_destination_3(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_4(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send without filtering and expect an error answer when specifying
%% an unknown host or realm.
send_destination_5(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    ?answer_message(?REALM_NOT_SERVED)
        = call(Config, Req).
send_destination_6(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, "unknown.org")]}],
    ?answer_message(?UNABLE_TO_DELIVER)
        = call(Config, Req).

%% Specify an invalid option and expect failure.
send_bad_option_1(Config) ->
    send_bad_option(Config, x).
send_bad_option_2(Config) ->
    send_bad_option(Config, {extra, false}).

send_bad_option(Config, Opt) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    try call(Config, Req, [Opt]) of
        T -> erlang:error({?MODULE, ?LINE, T})
    catch
        error: _ -> ok
    end.

%% Specify an invalid filter and expect no matching peers.
send_bad_filter_1(Config) ->
    send_bad_filter(Config, {all, none}).
send_bad_filter_2(Config) ->
    send_bad_filter(Config, {host, x}).
send_bad_filter_3(Config) ->
    send_bad_filter(Config, {eval, fun() -> true end}).
send_bad_filter_4(Config) ->
    send_bad_filter(Config, {eval, {?MODULE, not_exported, []}}).

send_bad_filter(Config, F) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{filter, F}]).

%% Specify multiple filter options and expect them be conjunctive.
send_multiple_filters_1(Config) ->
    Fun = fun(#diameter_caps{}) -> true end,
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = send_multiple_filters(Config, [host, {eval, Fun}]).
send_multiple_filters_2(Config) ->
    E = {erlang, is_tuple, []},
    {error, no_connection}
        = send_multiple_filters(Config, [realm, {neg, {eval, E}}]).
send_multiple_filters_3(Config) ->
    E1 = [fun(#diameter_caps{}, ok) -> true end, ok],
    E2 = {erlang, is_tuple, []},
    E3 = {erlang, is_record, [diameter_caps]},
    E4 = [{erlang, is_record, []}, diameter_caps],
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = send_multiple_filters(Config, [{eval, E} || E <- [E1,E2,E3,E4]]).

send_multiple_filters(Config, Fs) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    call(Config, Req, [{filter, F} || F <- Fs]).

%% Ensure that we can pass a request in any form to diameter:call/4,
%% only the return value from the prepare_request callback being
%% significant.
send_anything(Config) ->
    ['STA', {'Session-Id', _}, {'Result-Code', ?SUCCESS} | _]
        = call(Config, anything).

%% ===========================================================================

group(Config) ->
    #group{} = proplists:get_value(group, Config).

string(V, Config) ->
    #group{client_strings = B} = group(Config),
    decode(V,B).

decode(S, true)
  when is_list(S) ->
    S;
decode(B, false)
  when is_binary(B) ->
    binary_to_list(B).

call(Config, Req) ->
    call(Config, Req, []).

call(Config, Req, Opts) ->
    Name = proplists:get_value(testcase, Config),
    #group{client_service = CN,
           client_encoding = ReqEncoding,
           client_dict0 = Dict0}
        = Group
        = group(Config),
    diameter:call(CN,
                  dict(Req, Dict0),
                  msg(Req, ReqEncoding, Dict0),
                  [{extra, [{Name, Group}, diameter_lib:now()]} | Opts]).

origin({A,C}) ->
    2*codec(A) + container(C);

origin(N) ->
    {codec(N band 2), container(N rem 2)}.

%% Map booleans, but the readable atoms are part of (constructed)
%% group names, so it's good that they're readable.

codec(record) -> 0;
codec(list)   -> 1;
codec(0) -> record;
codec(_) -> list.

container(pkt) -> 0;
container(msg) -> 1;
container(0) -> pkt;
container(_) -> msg.

msg([H|_] = Msg, record = E, diameter_gen_base_rfc3588)
  when H == 'ACR';
       H == 'ACA' ->
    msg(Msg, E, diameter_gen_base_accounting);
msg([H|_] = Msg, record = E, diameter_gen_base_rfc6733)
  when H == 'ACR';
       H == 'ACA' ->
    msg(Msg, E, diameter_gen_acct_rfc6733);
msg([H|T], record, Dict) ->
    Dict:'#new-'(Dict:msg2rec(H), T);
msg(Msg, _, _) ->
    Msg.

dict0(D) ->
    ?A("diameter_gen_base_" ++ ?L(D)).

dict(Msg, Dict0)
  when 'ACR' == hd(Msg);
       'ACA' == hd(Msg);
       ?is_record(Msg, diameter_base_accounting_ACR);
       ?is_record(Msg, diameter_base_accounting_ACA) ->
    acct(Dict0);
dict(_, Dict0) ->
    Dict0.

acct(diameter_gen_base_rfc3588) ->
    diameter_gen_base_accounting;
acct(diameter_gen_base_rfc6733) ->
    diameter_gen_acct_rfc6733.

%% Set only values that aren't already.
set(_, [H|T], Vs) ->
    [H | Vs ++ T];
set(#group{client_dict0 = Dict0} = _Group, Rec, Vs) ->
    Dict = dict(Rec, Dict0),
    lists:foldl(fun({F,_} = FV, A) ->
                        set(Dict, Dict:'#get-'(F, A), FV, A)
                end,
                Rec,
                Vs).

set(Dict, E, FV, Rec)
  when E == undefined;
       E == [] ->
    Dict:'#set-'(FV, Rec);
set(_, _, _, Rec) ->
    Rec.

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/6-7

pick_peer(Peers, _, [$C|_], _State, {Name, Group}, _)
  when Name /= send_detach ->
    find(Group, Peers).

pick_peer(_Peers, _, [$C|_], _State, {send_nopeer, _}, _, ?EXTRA) ->
    false;

pick_peer(Peers, _, [$C|_], _State, {send_detach, Group}, _, {_,_}) ->
    find(Group, Peers).

find(#group{client_service = CN,
            server_encoding = A,
            server_container = C},
     Peers) ->
    Id = {A,C},
    [P] = [P || P <- Peers, id(Id, P, CN)],
    {ok, P}.

id(Id, {Pid, _Caps}, SvcName) ->
    [{ref, _}, {type, _}, {options, Opts} | _]
        = diameter:service_info(SvcName, Pid),
    lists:member({id, Id}, Opts).

%% prepare_request/5-6

prepare_request(_Pkt, [$C|_], {_Ref, _Caps}, {send_discard, _}, _) ->
    {discard, unprepared};

prepare_request(Pkt, [$C|_], {_Ref, Caps}, {Name, Group}, _) ->
    {send, prepare(Pkt, Caps, Name, Group)}.

prepare_request(Pkt, [$C|_], {_Ref, Caps}, {send_detach, Group}, _, _) ->
    {eval_packet, {send, prepare(Pkt, Caps, Group)}, [fun log/2, detach]}.

log(#diameter_packet{bin = Bin} = P, T)
  when is_binary(Bin) ->
    io:format("~p: ~p~n", [T,P]).

%% prepare/4

prepare(Pkt, Caps, N, #group{client_dict0 = Dict0} = Group)
  when N == send_unknown_short_mandatory;
       N == send_unknown_short ->
    Req = prepare(Pkt, Caps, Group),

    #diameter_packet{header = #diameter_header{length = L},
                     bin = Bin}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),

    %% Find the unknown AVP data at the end of the message and alter
    %% its length header.

    {Padding, [17|_]} = lists:splitwith(fun(C) -> C == 0 end,
                                       lists:reverse(binary_to_list(Bin))),

    Offset = L - length(Padding) - 4,
    <<H:Offset/binary, Len:24, T/binary>> = Bin,
    E#diameter_packet{bin = <<H/binary, (Len+9):24, T/binary>>};

prepare(Pkt, Caps, N, #group{client_dict0 = Dict0} = Group)
  when N == send_long_avp_length;
       N == send_short_avp_length;
       N == send_zero_avp_length ->
    Req = prepare(Pkt, Caps, Group),
    %% Second last AVP in our STR is Auth-Application-Id of type
    %% Unsigned32: set AVP Length to a value other than 12 and place
    %% it last in the message (so as not to mess with Termination-Cause).
    #diameter_packet{header = #diameter_header{length = L},
                     bin = B}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    Offset = L - 24,  %% to Auth-Application-Id
    <<H:Offset/binary,
      Hdr:5/binary, 12:24, Data:4/binary,
      T:12/binary>>
        = B,
    AL = case N of
             send_long_avp_length  -> 13;
             send_short_avp_length -> 11;
             send_zero_avp_length  -> 0
         end,
    E#diameter_packet{bin = <<H/binary,
                              T/binary,
                              Hdr/binary, AL:24, Data/binary>>};

prepare(Pkt, Caps, N, #group{client_dict0 = Dict0} = Group)
  when N == send_invalid_avp_length;
       N == send_invalid_reject ->
    Req = prepare(Pkt, Caps, Group),
    %% Second last AVP in our STR is Auth-Application-Id of type
    %% Unsigned32: send data of length 8.
    #diameter_packet{header = #diameter_header{length = L},
                     bin = B0}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    Offset = L - 7 - 12,  %% to AVP Length
    <<H0:Offset/binary, 12:24, T:16/binary>> = B0,
    <<V, L:24, H/binary>> = H0,  %% assert
    E#diameter_packet{bin = <<V, (L+4):24, H/binary, 16:24, 0:32, T/binary>>};

prepare(Pkt, Caps, send_unexpected_mandatory, #group{client_dict0 = Dict0}
                                              = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<V, Len:24, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    {Code, Flags, undefined} = Dict0:avp_header('Proxy-State'),
    Avp = <<Code:32, Flags, 8:24>>,
    E#diameter_packet{bin = <<V, (Len+8):24, T/binary, Avp/binary>>};

prepare(Pkt, Caps, send_grouped_error, #group{client_dict0 = Dict0}
                                              = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = Bin}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    {Code, Flags, undefined} = Dict0:avp_header('Proxy-Info'),
    %% Find Proxy-Info by looking for its header.
    Pattern = <<Code:32, Flags, 28:24>>,
    {Offset, 8} = binary:match(Bin, Pattern),

    %% Extract and swap Proxy-Host/State payloads.

    <<H:Offset/binary,
      PI:8/binary,
      PH:5/binary,
      12:24,
      Payload:4/binary,
      PS:5/binary,
      8:24,
      T/binary>>
        = Bin,

    E#diameter_packet{bin = <<H/binary,
                              PI/binary,
                              PH/binary,
                              8:24,
                              PS:5/binary,
                              12:24,
                              Payload/binary,
                              T/binary>>};

prepare(Pkt, Caps, send_unsupported, #group{client_dict0 = Dict0} = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<H:5/binary, _CmdCode:3/binary, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, 42:24, T/binary>>};

prepare(Pkt, Caps, send_unsupported_app, #group{client_dict0 = Dict0}
                                         = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<H:8/binary, _ApplId:4/binary, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, ?BAD_APP:32, T/binary>>};

prepare(Pkt, Caps, send_error_bit, Group) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{is_error = true},
                        msg = prepare(Pkt, Caps, Group)};

prepare(Pkt, Caps, send_unsupported_version, Group) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{version = 42},
                        msg = prepare(Pkt, Caps, Group)};

prepare(Pkt, Caps, send_anything, Group) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    prepare(Pkt#diameter_packet{msg = Req}, Caps, Group);

prepare(Pkt, Caps, _Name, Group) ->
    prepare(Pkt, Caps, Group).

%% prepare/3

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_accounting_ACR);
       'ACR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,

    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Realm', DR}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_ASR);
       'ASR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Host',  DH},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_STR);
       'STR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_RAR);
       'RAR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Host',  DH},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]).

%% prepare_retransmit/5

prepare_retransmit(_Pkt, false, _Peer, _Name, _Group) ->
    discard.

%% handle_answer/6-7

handle_answer(Pkt, Req, [$C|_], Peer, {Name, Group}, _) ->
    answer(Pkt, Req, Peer, Name, Group).

handle_answer(Pkt, Req, [$C|_], Peer, {send_detach = Name, Group}, _, X) ->
    {Pid, Ref} = X,
    Pid ! {Ref, answer(Pkt, Req, Peer, Name, Group)}.

answer(Pkt, Req, _Peer, Name, #group{client_dict0 = Dict0}) ->
    #diameter_packet{header = H, msg = Ans, errors = Es} = Pkt,
    ApplId = app(Req, Name, Dict0),
    #diameter_header{application_id = ApplId} = H,  %% assert
    Dict = dict(Ans, Dict0),
    [R | Vs] = Dict:'#get-'(answer(Ans, Es, Name)),
    [Dict:rec2msg(R) | Vs].

%% Missing Result-Code and inappropriate Experimental-Result-Code.
answer(Rec, Es, send_experimental_result) ->
    [{5004, #diameter_avp{name = 'Experimental-Result'}},
     {5005, #diameter_avp{name = 'Result-Code'}}]
        =  Es,
    Rec;

%% An inappropriate E-bit results in a decode error ...
answer(Rec, Es, send_bad_answer) ->
    [{5004, #diameter_avp{name = 'Result-Code'}} | _] = Es,
    Rec;

%% ... while other errors are reflected in Failed-AVP.
answer(Rec, [], _) ->
    Rec.

app(_, send_unsupported_app, _) ->
    ?BAD_APP;
app(Req, _, Dict0) ->
    Dict = dict(Req, Dict0),
    Dict:id().

%% handle_error/6

handle_error(timeout = Reason, _Req, [$C|_], _Peer, _, Time) ->
    Now = diameter_lib:now(),
    {Reason, {diameter_lib:timestamp(Time),
              diameter_lib:timestamp(Now),
              diameter_lib:micro_diff(Now, Time)}};

handle_error(Reason, _Req, [$C|_], _Peer, _, _Time) ->
    {error, Reason}.

%% handle_request/3

%% Note that diameter will set Result-Code and Failed-AVPs if
%% #diameter_packet.errors is non-null.

handle_request(#diameter_packet{header = H, msg = M, avps = As},
               _,
               {_Ref, Caps}) ->
    #diameter_header{end_to_end_id = EI,
                     hop_by_hop_id = HI}
        = H,
    {V,B} = ?CLIENT_MASK,
    V = EI bsr B,  %% assert
    V = HI bsr B,  %%
    #diameter_caps{origin_state_id = {_,[Id]}} = Caps,
    answer(origin(Id), request(M, [H|As], Caps)).

answer(T, {Tag, Action, Post}) ->
    {Tag, answer(T, Action), Post};
answer(_, {reply, [#diameter_header{} | _]} = T) ->
    T;
answer({A,C}, {reply, Ans}) ->
    answer(C, {reply, msg(Ans, A, diameter_gen_base_rfc3588)});
answer(pkt, {reply, Ans})
  when not is_record(Ans, diameter_packet) ->
    {reply, #diameter_packet{msg = Ans}};
answer(_, T) ->
    T.

%% request/3

%% send_experimental_result
request(#diameter_base_accounting_ACR{'Accounting-Record-Number' = 5},
        [Hdr | Avps],
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    [H,R|T] = [A || N <- ['Origin-Host',
                          'Origin-Realm',
                          'Session-Id',
                          'Accounting-Record-Type',
                          'Accounting-Record-Number'],
                    #diameter_avp{} = A
                        <- [lists:keyfind(N, #diameter_avp.name, Avps)]],
    Ans = [Hdr#diameter_header{is_request = false},
           H#diameter_avp{data = OH},
           R#diameter_avp{data = OR},
           #diameter_avp{name = 'Experimental-Result',
                         code = 297,
                         need_encryption = false,
                         data = [#diameter_avp{data = {?DIAMETER_DICT_COMMON,
                                                       'Vendor-Id',
                                                       123}},
                                 #diameter_avp{data
                                               = {?DIAMETER_DICT_COMMON,
                                                  'Experimental-Result-Code',
                                                  3987}}]}
           | T],
    {reply, Ans};

request(Msg, _Avps, Caps) ->
    request(Msg, Caps).

%% request/2

%% send_nok
request(#diameter_base_accounting_ACR{'Accounting-Record-Number' = 0},
        _) ->
    {eval_packet, {protocol_error, ?INVALID_AVP_BITS}, [fun log/2, invalid]};

%% send_bad_answer
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 2 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['ACA', {'Result-Code', ?SUCCESS},
                  {'Session-Id', SId},
                  {'Origin-Host', OH},
                  {'Origin-Realm', OR},
                  {'Accounting-Record-Type', RT},
                  {'Accounting-Record-Number', RN}],

    {reply, #diameter_packet{header = #diameter_header{is_error = true},%% NOT
                             msg = Ans}};

%% send_eval
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 3 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['ACA', {'Result-Code', ?SUCCESS},
                  {'Session-Id', SId},
                  {'Origin-Host', OH},
                  {'Origin-Realm', OR},
                  {'Accounting-Record-Type', RT},
                  {'Accounting-Record-Number', RN}],
    {eval, {reply, Ans}, {erlang, now, []}};

%% send_ok
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 1 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ACA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'Accounting-Record-Type', RT},
                    {'Accounting-Record-Number', RN}]};

%% send_protocol_error
request(#diameter_base_accounting_ACR{'Accounting-Record-Number' = 4},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['answer-message', {'Result-Code', ?TOO_BUSY},
                             {'Origin-Host', OH},
                             {'Origin-Realm', OR}],
    {reply, Ans};

request(#diameter_base_ASR{'Session-Id' = SId,
                           'AVP' = Avps},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ASA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'AVP', Avps}]};

%% send_invalid_reject
request(#diameter_base_STR{'Termination-Cause' = ?USER_MOVED},
        _Caps) ->
    {protocol_error, ?TOO_BUSY};

%% send_noreply
request(#diameter_base_STR{'Termination-Cause' = T},
        _Caps)
  when T /= ?LOGOUT ->
    discard;

%% send_destination_5
request(#diameter_base_STR{'Destination-Realm' = R},
        #diameter_caps{origin_realm = {OR, _}})
  when R /= undefined, R /= OR ->
    {protocol_error, ?REALM_NOT_SERVED};

%% send_destination_6
request(#diameter_base_STR{'Destination-Host' = [H]},
        #diameter_caps{origin_host = {OH, _}})
  when H /= OH ->
    {protocol_error, ?UNABLE_TO_DELIVER};

request(#diameter_base_STR{'Session-Id' = SId},
        #diameter_caps{origin_host  = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['STA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR}]};

%% send_error/send_timeout
request(#diameter_base_RAR{}, _Caps) ->
    receive after 2000 -> {protocol_error, ?TOO_BUSY} end.
