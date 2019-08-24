%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2019. All Rights Reserved.
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
%% The traffic isn't meant to be sensible, just to exercise code.
%%

-module(diameter_traffic_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([rfc4005/1,
         start/1,
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
         send_proxy_info/1,
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
         send_too_many/1,
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
         remove_transports/1,
         empty/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/4,
         peer_down/4,
         pick_peer/7, pick_peer/8,
         prepare_request/6, prepare_request/7,
         prepare_retransmit/6,
         handle_answer/7, handle_answer/8,
         handle_error/7,
         handle_request/4]).

%% diameter_{tcp,sctp} callbacks
-export([message/3]).

-include_lib("kernel/include/inet_sctp.hrl").

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").
-include("diameter_gen_base_accounting.hrl").
%% The listening transports use RFC 3588 dictionaries, the client
%% transports use either 3588 or 6733. (So can't use the record
%% definitions in the latter case.)

%% ===========================================================================

%% Fraction of shuffle/parallel groups to randomly skip.
-define(SKIP, 0.25).

%% Positive number of testcases from which to select (randomly) from
%% tc(), the list of testcases to run, or [] to run all. The random
%% selection is to limit the time it takes for the suite to run.
-define(LIMIT, #{tcp => 42, sctp => 5}).

-define(util, diameter_util).

-define(A, list_to_atom).
-define(L, atom_to_list).
-define(B, iolist_to_binary).

%% Don't use is_record/2 since dictionary hrl's aren't included.
%% (Since they define conflicting records with the same names.)
-define(is_record(Rec, Name), (Name == element(1, Rec))).

-define(ADDR, {127,0,0,1}).

-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).

-define(EXTRA, an_extra_argument).

%% Sequence mask for End-to-End and Hop-by-Hop identifiers.
-define(CLIENT_MASK, {1,26}).  %% 1 in top 6 bits

%% How to construct outgoing messages.
-define(ENCODINGS, [list, record, map]).

%% How to decode incoming messages.
-define(DECODINGS, [record, none, map, list, record_from_map]).

%% Which dictionary to use in the clients.
-define(RFCS, [rfc3588, rfc6733, rfc4005]).

%% Whether to decode stringish Diameter types to strings, or leave
%% them as binary.
-define(STRING_DECODES, [false, true]).

%% Which transport protocol to use.
-define(TRANSPORTS, [tcp, sctp]).

%% Send from a dedicated process?
-define(SENDERS, [true, false]).

%% Message callbacks from diameter_{tcp,sctp}?
-define(CALLBACKS, [true, false]).

-record(group,
        {transport,
         strings,
         encoding,
         client_service,
         client_dict,
         client_sender,
         server_service,
         server_decoding,
         server_sender,
         server_throttle}).

%% Not really what we should be setting unless the message is sent in
%% the common application but diameter doesn't care.
-define(APP_ID, ?DIAMETER_APP_ID_COMMON).

%% An Application-ID the server doesn't support.
-define(BAD_APP, 42).

%% A common match when receiving answers in a client.
-define(answer_message(SessionId, ResultCode),
        ['answer-message' | #{'Session-Id' := SessionId,
                              'Origin-Host' := _,
                              'Origin-Realm' := _,
                              'Result-Code' := ResultCode}]).
-define(answer_message(ResultCode),
        ['answer-message' | #{'Origin-Host' := _,
                              'Origin-Realm' := _,
                              'Result-Code' := ResultCode}]).

%% Config for diameter:start_service/2.
-define(SERVICE(Name, Grp),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [0]},  %% common messages
         {'Acct-Application-Id', [3]},  %% base accounting
         {restrict_connections, false},
         {string_decode, Grp#group.strings},
         {avp_dictionaries, [diameter_gen_doic_rfc7683]},
         {incoming_maxlen, 1 bsl 21}
         | [{application, [{dictionary, D},
                           {module, [?MODULE, Grp]},
                           {answer_errors, callback}]}
            || D <- [diameter_gen_base_rfc3588,
                     diameter_gen_base_accounting,
                     diameter_gen_base_rfc6733,
                     diameter_gen_acct_rfc6733,
                     nas4005],
               D /= nas4005 orelse have_nas()]]).

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
-define(TOO_MANY,
        ?'DIAMETER_BASE_RESULT-CODE_AVP_OCCURS_TOO_MANY_TIMES').
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
    [rfc4005, start, result_codes, {group, traffic}, empty, stop].

%% Redefine this to run one or more groups for debugging purposes.
-define(GROUPS, []).
%-define(GROUPS, [[tcp,rfc6733,record,map,false,false,false,false]]).

%% Issues with gen_sctp sporadically cause huge numbers of failed
%% testcases when running testcases in parallel.
groups() ->
    Names = names(),
    [{P, [P], Ts} || Ts <- [tc()], P <- [shuffle, parallel]]
        ++
        [{?util:name(N), [], [{group, if T == sctp; S -> shuffle;
                                         true         -> parallel end}]}
         || [T,_,_,_,S|_] = N <- Names]
        ++
        [{T, [], [{group, ?util:name(N)} || N <- names(Names, ?GROUPS),
                                            T == hd(N)]}
         || T <- ?TRANSPORTS]
        ++
        [{traffic, [], [{group, T} || T <- ?TRANSPORTS]}].

names() ->
    [[T,R,E,D,S,ST,SS,CS] || T  <- ?TRANSPORTS,
                             R  <- ?RFCS,
                             E  <- ?ENCODINGS,
                             D  <- ?DECODINGS,
                             S  <- ?STRING_DECODES,
                             ST <- ?CALLBACKS,
                             SS <- ?SENDERS,
                             CS <- ?SENDERS].

names(Names, []) ->
    [N || N <- Names,
          [CS,SS|_] <- [lists:reverse(N)],
          SS orelse CS];  %% avoid deadlock

names(_, Names) ->
    Names.

%% --------------------

init_per_suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    [{rfc4005, compile_and_load()}, {sctp, ?util:have_sctp()} | Config].

end_per_suite(_Config) ->
    code:delete(nas4005),
    code:purge(nas4005),
    ok.

%% --------------------

init_per_group(_) ->
    [{timetrap, {seconds, 30}}].

init_per_group(Name, Config)
  when Name == shuffle;
       Name == parallel ->
    case rand:uniform() < ?SKIP of
        true ->
            {skip, random};
        false ->
            start_services(Config),
            add_transports(Config),
            replace({sleep, Name == parallel}, Config)
    end;

init_per_group(sctp = Name, Config) ->
    {_, Sctp} = lists:keyfind(Name, 1, Config),
    if Sctp ->
            Config;
       true ->
            {skip, Name}
    end;

init_per_group(Name, Config) ->
    Nas = proplists:get_value(rfc4005, Config, false),
    case ?util:name(Name) of
        [_,R,_,_,_,_,_,_] when R == rfc4005, true /= Nas ->
            {skip, rfc4005};
        [T,R,E,D,S,ST,SS,CS] ->
            G = #group{transport = T,
                       strings = S,
                       encoding = E,
                       client_service = [$C|?util:unique_string()],
                       client_dict = appdict(R),
                       client_sender = CS,
                       server_service = [$S|?util:unique_string()],
                       server_decoding = D,
                       server_sender = SS,
                       server_throttle = ST},
            replace([{group, G}, {runlist, select(T)}], Config);
        _ ->
            Config
    end.

end_per_group(Name, Config)
  when Name == shuffle;
       Name == parallel ->
    remove_transports(Config),
    stop_services(Config);

end_per_group(_, _) ->
    ok.

select(T) ->
    try maps:get(T, ?LIMIT) of
        N ->
            lists:sublist(?util:scramble(tc()), max(5, rand:uniform(N)))
    catch
        error:_ -> ?LIMIT
    end.

%% --------------------

%% Work around common_test accumulating Config improperly, causing
%% testcases to get Config from groups and suites they're not in.
init_per_testcase(N, Config)
  when N == rfc4005;
       N == start;
       N == result_codes;
       N == empty;
       N == stop ->
    Config;

%% Skip testcases that can reasonably fail under SCTP.
init_per_testcase(Name, Config) ->
    TCs = proplists:get_value(runlist, Config, []),
    Run = [] == TCs orelse lists:member(Name, TCs),
    case [G || #group{transport = sctp} = G
                   <- [proplists:get_value(group, Config)]]
    of
        [_] when Name == send_maxlen;
                 Name == send_long ->
            {skip, sctp};
        _ when not Run ->
            {skip, random};
        _ ->
            proplists:get_value(sleep, Config, false)
                andalso timer:sleep(rand:uniform(200)),
            [{testcase, Name} | Config]
    end.

end_per_testcase(_, _) ->
    ok.

%% replace/2
%%
%% Work around common_test running init functions inappropriately, and
%% this accumulating more config than expected.

replace(Pairs, Config)
  when is_list(Pairs) ->
    lists:foldl(fun replace/2, Config, Pairs);

replace({Key, _} = T, Config) ->
    [T | lists:keydelete(Key, 1, Config)].

%% --------------------

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
     send_proxy_info,
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
     send_too_many,
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
           server_service = SN,
           server_decoding = SD}
        = Grp
        = group(Config),
    ok = diameter:start_service(SN, [{traffic_counters, bool()},
                                     {decode_format, SD}
                                     | ?SERVICE(SN, Grp)]),
    ok = diameter:start_service(CN, [{traffic_counters, bool()},
                                     {sequence, ?CLIENT_MASK},
                                     {decode_format, map},
                                     {strict_arities, decode}
                                     | ?SERVICE(CN, Grp)]).

bool() ->
    0.5 =< rand:uniform().

add_transports(Config) ->
    #group{transport = T,
           encoding = E,
           client_service = CN,
           client_sender = CS,
           server_service = SN,
           server_sender = SS,
           server_throttle = ST}
        = group(Config),
    LRef = ?util:listen(SN,
                        [T,
                         {sender, SS},
                         {message_cb, ST andalso {?MODULE, message, [0]}}]
                        ++ [{packet, hd(?util:scramble([false, raw]))}
                            || T == sctp andalso CS]
                        ++ [{unordered, unordered()} || T == sctp],
                        [{capabilities_cb, fun capx/2},
                         {pool_size, 8}
                         | server_apps()]),
    Cs = [?util:connect(CN,
                        [T, {sender, CS} | client_opts(T)],
                        LRef,
                        [{id, Id}
                         | client_apps(R, [{'Origin-State-Id', origin(Id)}])])
          || D <- ?DECODINGS,  %% for multiple candidate peers
             R <- ?RFCS,
             R /= rfc4005 orelse have_nas(),
             Id <- [{D,E}]],
    ?util:write_priv(Config, "transport", [LRef | Cs]).

unordered() ->
    element(rand:uniform(4), {true, false, 1, 2}).

client_opts(tcp) ->
    [];
client_opts(sctp) ->
    [{unordered, unordered()}
     | [{sctp_initmsg, #sctp_initmsg{num_ostreams = N,
                                     max_instreams = 5}}
        || N <- [rand:uniform(8)],
           N =< 6]].

server_apps() ->
    B = have_nas(),
    [{applications, [diameter_gen_base_rfc3588,
                     diameter_gen_base_accounting]
                    ++ [nas4005 || B]},
     {capabilities, [{'Auth-Application-Id', [0] ++ [1 || B]}, %% common, NAS
                     {'Acct-Application-Id', [3]}]}].          %% accounting

client_apps(D, Caps) ->
    if D == rfc4005 ->
            [{applications, [nas4005]},
             {capabilities, [{'Auth-Application-Id', [1]},     %% NAS
                             {'Acct-Application-Id', []}
                             | Caps]}];
       true ->
            D0 = dict0(D),
            [{applications, [acct(D0), D0]},
             {capabilities, Caps}]
    end.

have_nas() ->
    false /= code:is_loaded(nas4005).

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

%% Fail only this testcase if the RFC 4005 dictionary hasn't been
%% successfully compiled and loaded.
rfc4005(Config) ->
    true = proplists:get_value(rfc4005, Config).

%% Ensure that result codes have the expected values.
result_codes(_Config) ->
    {2001,
     3001, 3002, 3003, 3004, 3007, 3008, 3009,
     5001, 5009, 5011, 5014}
        = {?SUCCESS,
           ?COMMAND_UNSUPPORTED,
           ?UNABLE_TO_DELIVER,
           ?REALM_NOT_SERVED,
           ?TOO_BUSY,
           ?APPLICATION_UNSUPPORTED,
           ?INVALID_HDR_BITS,
           ?INVALID_AVP_BITS,
           ?AVP_UNSUPPORTED,
           ?TOO_MANY,
           ?UNSUPPORTED_VERSION,
           ?INVALID_AVP_LENGTH}.

%% Send an ACR and expect success.
send_ok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 1}],
    ['ACA' | #{'Result-Code' := ?SUCCESS,
               'Session-Id' := _}]
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

    ['ACA' | #{'Result-Code' := ?SUCCESS,
               'Session-Id' := _}]
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
%% protocol error and some AVPs to check the decoding of.
send_protocol_error(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 4}],

    ['answer-message' | #{'Result-Code' := ?TOO_BUSY,
                          'AVP' := [OLR | _]} = Avps]
        = call(Config, Req),

    #diameter_avp{name = 'OC-OLR',
                  value = #{'OC-Sequence-Number' := 1,
                            'OC-Report-Type' := 0,  %% HOST_REPORT
                            'OC-Reduction-Percentage' := [25],
                            'OC-Validity-Duration' := [60],
                            'AVP' := [OSF]}}
        = OLR,
    #diameter_avp{name = 'OC-Supported-Features',
                  value = #{} = Fs}
        = OSF,
    0 = maps:size(Fs),

    #group{client_dict = D} = group(Config), 

    if D == nas4005 ->
            error = maps:find('Failed-AVP', Avps),
            #{'AVP' := [_,Failed]}
                = Avps,
            #diameter_avp{name = 'Failed-AVP',
                          value = #{'AVP' := [NP,FR,AP]}}
                = Failed,
            #diameter_avp{name = 'NAS-Port',
                          value = 44}
                = NP,
            #diameter_avp{name = 'Firmware-Revision',
                          value = 12}
                = FR,
            #diameter_avp{name = 'Auth-Grace-Period',
                          value = 13}
                = AP;

       D == diameter_gen_base_rfc3588;
       D == diameter_gen_basr_accounting ->
            error = maps:find('Failed-AVP', Avps),
            #{'AVP' := [_,Failed]}
                = Avps,

            #diameter_avp{name = 'Failed-AVP',
                           value = #{'AVP' := [NP,FR,AP]}}
                = Failed,
            #diameter_avp{name = undefined,
                          value = undefined}
                = NP,
            #diameter_avp{name = 'Firmware-Revision',
                          value = 12}
                = FR,
            #diameter_avp{name = 'Auth-Grace-Period',
                          value = 13}
                = AP;

       D == diameter_gen_base_rfc6733;
       D == diameter_gen_acct_rfc6733 ->
            #{'Failed-AVP' := [#{'AVP' := [NP,FR,AP]}],
              'AVP' := [_]}
                = Avps,
            #diameter_avp{name = undefined,
                          value = undefined}
                = NP,
            #diameter_avp{name = 'Firmware-Revision',
                          value = 12}
                = FR,
            #diameter_avp{name = 'Auth-Grace-Period',
                          value = 13}
                = AP
    end.

%% Send a 3xxx Experimental-Result in an answer not setting the E-bit
%% and missing a Result-Code.
send_experimental_result(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 5}],
    ['ACA' | #{'Session-Id' := _}]
        = call(Config, Req).

%% Send an ASR with an arbitrary non-mandatory AVP and expect success
%% and the same AVP in the reply.
send_arbitrary(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{name = 'Product-Name',
                                         value = "XXX"}]}],
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS,
               'AVP' := [#diameter_avp{name = 'Product-Name',
                                       value = V}]}]
        = call(Config, Req),
    "XXX" = string(V, Config).

%% Send Proxy-Info in an ASR that the peer answers with 3xxx, and
%% ensure that the AVP is returned.
send_proxy_info(Config) ->
    H0 = ?B(?util:unique_string()),
    S0 = ?B(?util:unique_string()),
    Req = ['ASR', {'Proxy-Info', #{'Proxy-Host'  => H0,
                                   'Proxy-State' => S0}}],
    ['answer-message' | #{'Result-Code' := 3999,
                          'Proxy-Info' := [#{'Proxy-Host' := H,
                                             'Proxy-State' := S}]}]
        = call(Config, Req),
    [H0, S0] = [?B(X) || X <- [H,S]].

%% Send an unknown AVP (to some client) and check that it comes back.
send_unknown(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = false,
                                         data = <<17>>}]}],
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS,
               'AVP' := [#diameter_avp{code = 999,
                                       is_mandatory = false,
                                       data = <<17>>}]}]
        = call(Config, Req).

%% Ditto, and point the AVP length past the end of the message. Expect
%% 5014.
send_unknown_short(Config) ->
    send_unknown_short(Config, false, ?INVALID_AVP_LENGTH).

send_unknown_short(Config, M, RC) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = M,
                                         data = <<17>>}]}],
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := RC,
               'Failed-AVP' := [#{'AVP' := [Avp]}]}]
        = call(Config, Req),
    #diameter_avp{code = 999,
                  is_mandatory = M,
                  data = <<17, _/binary>>} %% extra bits from padding
        = Avp.

%% Ditto but set the M flag.
send_unknown_mandatory(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = true,
                                         data = <<17>>}]}],
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?AVP_UNSUPPORTED,
               'Failed-AVP' := [#{'AVP' := [Avp]}]}]
        = call(Config, Req),
    #diameter_avp{code = 999,
                  is_mandatory = true,
                  data = <<17>>}
        = Avp.

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
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?AVP_UNSUPPORTED,
               'Failed-AVP' := [#{'AVP' := [Avp]}]}]
        = call(Config, Req),
    #diameter_avp{code = 27,
                  is_mandatory = true,
                  value = 12,
                  data = <<12:32>>}
        = Avp.

%% Try to two Auth-Application-Id in ASR expect 5009.
send_too_many(Config) ->
    Req = ['ASR', {'Auth-Application-Id', [?APP_ID, 44]}],

    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?TOO_MANY,
               'Failed-AVP' := [#{'AVP' := [Avp]}]}]
        = call(Config, Req),
    #diameter_avp{name = 'Auth-Application-Id',
                  value = 44}
        = Avp.

%% Send an containing a faulty Grouped AVP (empty Proxy-Host in
%% Proxy-Info) and expect that only the faulty AVP is sent in
%% Failed-AVP. The encoded values of Proxy-Host and Proxy-State are
%% swapped in prepare_request since an empty Proxy-Host is an encode
%% error.
send_grouped_error(Config) ->
    Req = ['ASR', {'Proxy-Info', [[{'Proxy-Host', "abcd"},
                                   {'Proxy-State', ""}]]}],
    ['ASA' | #{'Session-Id' := _,
               'Result-Code' := ?INVALID_AVP_LENGTH,
               'Failed-AVP' := [#{'AVP' := [Avp]}]}]
        = call(Config, Req),
    #diameter_avp{name = 'Proxy-Info', value = #{'Proxy-Host' := H}}
        = Avp,
    <<0>> = ?B(H).

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
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?UNSUPPORTED_VERSION}]
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

    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?INVALID_AVP_LENGTH,
               'Origin-Host' := _,
               'Origin-Realm' := _,
               'Failed-AVP' := [#{'AVP' := [_]}]}]
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

    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?AVP_UNSUPPORTED}]
        = call(Config, Req).

%% Send something long that will be fragmented by TCP.
send_long(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [binary:copy(<<$X>>, 1 bsl 20)]}],
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = call(Config, Req).

%% Send something longer than the configure incoming_maxlen.
send_maxlen(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [binary:copy(<<$X>>, 1 bsl 21)]}],
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
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
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
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = receive {Ref, T} -> T end.

%% Send a request which can't be encoded and expect {error, encode}.
send_encode_error(Config) ->
    {error, encode} = call(Config, ['STR', {'Termination-Cause', huh}]).

%% Send with filtering and expect success.
send_destination_1(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, ?REALM)]}],
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send with filtering on and expect failure when specifying an
%% unknown host or realm.
send_destination_3(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', <<"unknown.org">>}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_4(Config) ->
    #group{server_service = SN}
        = group(Config),
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(SN, ["unknown.org"])]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send without filtering and expect an error answer when specifying
%% an unknown host or realm.
send_destination_5(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', [<<"unknown.org">>]}],
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
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
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
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = send_multiple_filters(Config, [{eval, E} || E <- [E1,E2,E3,E4]]).

send_multiple_filters(Config, Fs) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    call(Config, Req, [{filter, F} || F <- Fs]).

%% Ensure that we can pass a request in any form to diameter:call/4,
%% only the return value from the prepare_request callback being
%% significant.
send_anything(Config) ->
    ['STA' | #{'Session-Id' := _,
               'Result-Code' := ?SUCCESS}]
        = call(Config, anything).

%% ===========================================================================

group(Config) ->
    #group{} = proplists:get_value(group, Config).

string(V, Config) ->
    #group{strings = B} = group(Config),
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
    #group{encoding = Enc,
           client_service = CN,
           client_dict = Dict0}
        = group(Config),
    diameter:call(CN,
                  dict(Req, Dict0),
                  msg(Req, Enc, Dict0),
                  [{extra, [Name, diameter_lib:now()]} | Opts]).

origin({D,E}) ->
    4*decode(D) + encode(E);

origin(N) ->
    {decode(N bsr 2), encode(N rem 4)}.

%% Map atoms. The atoms are part of (constructed) group names, so it's
%% good that they're readable.

decode(record) -> 0;
decode(list)   -> 1;
decode(map)    -> 2;
decode(none)   -> 3;
decode(record_from_map) -> 4;
decode(0) -> record;
decode(1) -> list;
decode(2) -> map;
decode(3) -> none;
decode(4) -> record_from_map.

encode(record) -> 0;
encode(list)   -> 1;
encode(map)    -> 2;
encode(0) -> record;
encode(1) -> list;
encode(2) -> map.

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

msg([H|As], map, _)
  when is_list(As) ->
    [H | maps:from_list(As)];

msg(Msg, _, _) ->
    Msg.

to_map(#diameter_packet{msg = [_MsgName | Avps] = Msg},
       #group{server_decoding = map})
  when is_map(Avps) ->
    Msg;

to_map(#diameter_packet{msg = [MsgName | Avps]},
       #group{server_decoding = list}) ->
    [MsgName | maps:from_list(Avps)];

to_map(#diameter_packet{header = H, msg = Rec},
       #group{server_decoding = D})
  when D == record;
       D == record_from_map ->
    rec_to_map(Rec, dict(H));

%% No record decode: do it ourselves.
to_map(#diameter_packet{header = H,
                        msg = Name,
                        bin = Bin},
      #group{server_decoding = none,
             strings = B}) ->
    Opts = #{decode_format => map,
             string_decode => B,
             avp_dictionaries => [diameter_gen_doic_rfc7683],
             strict_mbit => true,
             rfc => 6733},
    #diameter_packet{msg = [MsgName | _Map] = Msg}
        = diameter_codec:decode(dict(H), Opts, Bin),
    {MsgName, _} = {Name, Msg},  %% assert
    Msg.

dict(#diameter_header{application_id = Id,
                      cmd_code = Code}) ->
    if Id == 1 ->
            nas4005;
       Code == 271 ->
            diameter_gen_base_accounting;
       true ->
            diameter_gen_base_rfc3588
    end.

rec_to_map(Rec, Dict) ->
    [R | Vs] = Dict:'#get-'(Rec),
    [Dict:rec2msg(R) | maps:from_list([T || {_,V} = T <- Vs,
                                            V /= undefined,
                                            V /= []])].

appdict(rfc4005) ->
    nas4005;
appdict(D) ->
    dict0(D).

dict0(D) ->
    ?A("diameter_gen_base_" ++ ?L(D)).

dict(Msg, Dict) ->
    d(name(Msg), Dict).

d(N, nas4005 = D) ->
    if N == {list, 'answer-message'};
       N == {map, 'answer-message'};
       N == {record, 'diameter_base_answer-message'} ->
            diameter_gen_base_rfc3588;
       true ->
            D
    end;
d(N, Dict0)
  when N == {list, 'ACR'};
       N == {list, 'ACA'};
       N == {map, 'ACR'};
       N == {map, 'ACA'};
       N == {record, diameter_base_accounting_ACR};
       N == {record, diameter_base_accounting_ACA} ->
    acct(Dict0);
d(_, Dict0) ->
    Dict0.

acct(diameter_gen_base_rfc3588) ->
    diameter_gen_base_accounting;
acct(diameter_gen_base_rfc6733) ->
    diameter_gen_acct_rfc6733.

%% Set only values that aren't already.

set(_, [N | As], Vs) ->
    [N | if is_map(As) ->
                 maps:merge(maps:from_list(Vs), As);
            is_list(As) ->
                 Vs ++ As
         end];

set(#group{client_dict = Dict0} = _Group, Rec, Vs) ->
    Dict = dict(Rec, Dict0),
    lists:foldl(fun({F,_} = FV, A) ->
                        reset(Dict, Dict:'#get-'(F, A), FV, A)
                end,
                Rec,
                Vs).

reset(Dict, E, FV, Rec)
  when E == undefined;
       E == [] ->
    Dict:'#set-'(FV, Rec);

reset(_, _, _, Rec) ->
    Rec.

%% ===========================================================================
%% diameter callbacks

%% peer_up/4

peer_up(_SvcName, _Peer, State, _Group) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State, _Group) ->
    State.

%% pick_peer/7-8

pick_peer(Peers, _, [$C|_], _State, Group, Name, _)
  when Name /= send_detach ->
    find(Group, Peers).

pick_peer(_Peers, _, [$C|_], _State, _Group, send_nopeer, _, ?EXTRA) ->
    false;

pick_peer(Peers, _, [$C|_], _State, Group, send_detach, _, {_,_}) ->
    find(Group, Peers).

find(#group{encoding = E,
            client_service = CN,
            server_decoding = D},
     [_|_] = Peers) ->
    Id = {D,E},
    [P] = [P || P <- Peers, id(Id, P, CN)],
    {ok, P}.

id(Id, {Pid, _Caps}, SvcName) ->
    [{ref, _}, {type, _}, {options, Opts} | _]
        = diameter:service_info(SvcName, Pid),
    lists:member({id, Id}, Opts).

%% prepare_request/6-7

prepare_request(_Pkt, [$C|_], {_Ref, _Caps}, _, send_discard, _) ->
    {discard, unprepared};

prepare_request(Pkt, [$C|_], {_Ref, Caps}, Group, Name, _) ->
    {send, prepare(Pkt, Caps, Name, Group)}.

prepare_request(Pkt, [$C|_], {_Ref, Caps}, Group, send_detach, _, _) ->
    {eval_packet, {send, prepare(Pkt, Caps, Group)}, [fun log/2, detach]}.

log(#diameter_packet{bin = Bin} = P, T)
  when is_binary(Bin) ->
    io:format("~p: ~p~n", [T,P]).

%% prepare/4

prepare(Pkt, Caps, N, #group{client_dict = Dict0} = Group)
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

prepare(Pkt, Caps, N, #group{client_dict = Dict0} = Group)
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

prepare(Pkt, Caps, N, #group{client_dict = Dict0} = Group)
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

prepare(Pkt, Caps, send_unexpected_mandatory, #group{client_dict = Dict0}
                                              = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<V, Len:24, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    {Code, Flags, undefined} = Dict0:avp_header('Proxy-State'),
    Avp = <<Code:32, Flags, 8:24>>,
    E#diameter_packet{bin = <<V, (Len+8):24, T/binary, Avp/binary>>};

prepare(Pkt, Caps, send_grouped_error, #group{client_dict = Dict0}
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

prepare(Pkt, Caps, send_unsupported, #group{client_dict = Dict0} = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<H:5/binary, _CmdCode:3/binary, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, 42:24, T/binary>>};

prepare(Pkt, Caps, send_unsupported_app, #group{client_dict = Dict0}
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

prepare(#diameter_packet{msg = Req} = Pkt, Caps, Group) ->
    set(name(Req), Pkt, Caps, Group).

%% set/4

set(N, #diameter_packet{msg = Req}, Caps, Group)
  when N == {record, diameter_base_accounting_ACR};
       N == {record, nas_ACR};
       N == {map, 'ACR'};
       N == {list, 'ACR'} ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,

    set(Group, Req, [{'Session-Id', [diameter:session_id(OH)]},
                     {'Origin-Host',  [OH]},
                     {'Origin-Realm', [OR]},
                     {'Destination-Realm', [DR]}]);

set(N, #diameter_packet{msg = Req}, Caps, Group)
  when N == {record, diameter_base_ASR};
       N == {record, nas_ASR};
       N == {map, 'ASR'};
       N == {list, 'ASR'} ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', [diameter:session_id(OH)]},
                     {'Origin-Host',  [OH]},
                     {'Origin-Realm', [OR]},
                     {'Destination-Host',  [DH]},
                     {'Destination-Realm', [DR]},
                     {'Auth-Application-Id', ?APP_ID}]);

set(N, #diameter_packet{msg = Req}, Caps, Group)
  when N == {record, diameter_base_STR};
       N == {record, nas_STR};
       N == {map, 'STR'};
       N == {list, 'STR'} ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', [diameter:session_id(OH)]},
                     {'Origin-Host',  [OH]},
                     {'Origin-Realm', [OR]},
                     {'Destination-Realm', [DR]},
                     {'Auth-Application-Id', ?APP_ID}]);

set(N, #diameter_packet{msg = Req}, Caps, Group)
  when N == {record, diameter_base_RAR};
       N == {record, nas_RAR};
       N == {map, 'RAR'};
       N == {list, 'RAR'} ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', [diameter:session_id(OH)]},
                     {'Origin-Host',  [OH]},
                     {'Origin-Realm', [OR]},
                     {'Destination-Host',  [DH]},
                     {'Destination-Realm', [DR]},
                     {'Auth-Application-Id', ?APP_ID}]).

%% name/1

name([H|#{}]) ->
    {map, H};

name([H|_]) ->
    {list, H};

name(Rec) ->
    try
        {record, element(1, Rec)}
    catch
        error: badarg ->
            false
    end.

%% prepare_retransmit/6

prepare_retransmit(_Pkt, false, _Peer, _Group, _Name, _) ->
    discard.

%% handle_answer/7-8

handle_answer(Pkt, Req, [$C|_], Peer, Group, Name, _) ->
    answer(Pkt, Req, Peer, Name, Group).

handle_answer(Pkt, Req, [$C|_], Peer, Group, send_detach = Name, _, X) ->
    {Pid, Ref} = X,
    Pid ! {Ref, answer(Pkt, Req, Peer, Name, Group)}.

answer(Pkt, Req, _Peer, Name, #group{client_dict = Dict0}) ->
    #diameter_packet{header = H, msg = Ans, errors = Es} = Pkt,
    ApplId = app(Req, Name, Dict0),
    #diameter_header{application_id = ApplId} = H,  %% assert
    answer(Ans, Es, Name).

%% Missing Result-Code and inappropriate Experimental-Result-Code.
answer(Ans, Es, send_experimental_result) ->
    [{5004, #diameter_avp{name = 'Experimental-Result'}},
     {5005, #diameter_avp{name = 'Result-Code'}}]
        =  Es,
    Ans;

%% An inappropriate E-bit results in a decode error ...
answer(Ans, Es, send_bad_answer) ->
    [{5004, #diameter_avp{name = 'Result-Code'}} | _] = Es,
    Ans;

%% ... while other errors are reflected in Failed-AVP.
answer(Ans, [], _) ->
    Ans.

app(_, send_unsupported_app, _) ->
    ?BAD_APP;
app(Req, _, Dict0) ->
    Dict = dict(Req, Dict0),
    Dict:id().

%% handle_error/7

handle_error(timeout = Reason, _Req, [$C|_], _Peer, _, _, Time) ->
    Now = diameter_lib:now(),
    {Reason, {diameter_lib:timestamp(Time),
              diameter_lib:timestamp(Now),
              diameter_lib:micro_diff(Now, Time)}};

handle_error(Reason, _Req, [$C|_], _Peer, _, _, _Time) ->
    {error, Reason}.

%% handle_request/4

%% Note that diameter will set Result-Code and Failed-AVPs if
%% #diameter_packet.errors is non-null.

handle_request(#diameter_packet{header = H, avps = As}
               = Pkt,
               _,
               {_Ref, Caps},
               #group{encoding = E,
                      server_decoding = D}
               = Grp) ->
    #diameter_header{end_to_end_id = EI,
                     hop_by_hop_id = HI}
        = H,
    {V,B} = ?CLIENT_MASK,
    V = EI bsr B,  %% assert
    V = HI bsr B,  %%
    #diameter_caps{origin_state_id = {_,[Id]}} = Caps,
    {D,E} = T = origin(Id),  %% assert
    wrap(T, H, request(to_map(Pkt, Grp), [H|As], Caps)).

wrap(Id, H, {Tag, Action, Post}) ->
    {Tag, wrap(Id, H, Action), Post};

wrap(_, _, {reply, [#diameter_header{} | _]} = T) ->
    T;

wrap({_,E}, H, {reply, Ans}) ->
    Msg = base_to_nas(msg(Ans, E, diameter_gen_base_rfc3588), H),
    {reply, wrap(Msg)};

wrap(_, _, T) ->
    T.

%% Randomly wrap the answer in a diameter_packet.

wrap(#diameter_packet{} = Pkt) ->
    Pkt;

wrap(Msg) ->
    case rand:uniform(2) of
        1 -> #diameter_packet{msg = Msg};
        2 -> Msg
    end.

%% base_to_nas/2

base_to_nas(#diameter_packet{msg = Msg} = Pkt, H) ->
    Pkt#diameter_packet{msg = base_to_nas(Msg, H)};

base_to_nas(Rec, #diameter_header{application_id = 1})
  when is_tuple(Rec), not ?is_record(Rec, 'diameter_base_answer-message') ->
    D = case element(1, Rec) of
            diameter_base_accounting_ACA ->
                diameter_gen_base_accounting;
            _ ->
                diameter_gen_base_rfc3588
        end,
    [R | Values] = D:'#get-'(Rec),
    "diameter_base_" ++ N = ?L(R),
    Name = ?A("nas_" ++ if N == "accounting_ACA" ->
                                "ACA";
                           true ->
                                N
                        end),
    nas4005:'#new-'([Name | Values]);

base_to_nas(Msg, _) ->
    Msg.

%% request/3

%% send_experimental_result
request(['ACR' | #{'Accounting-Record-Number' := 5}],
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
request(['ACR' | #{'Accounting-Record-Number' := 0}],
        _) ->
    {eval_packet, {protocol_error, ?INVALID_AVP_BITS}, [fun log/2, invalid]};

%% send_bad_answer
request(['ACR' | #{'Session-Id' := SId,
                   'Accounting-Record-Type' := RT,
                   'Accounting-Record-Number' := 2 = RN}],
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
request(['ACR' | #{'Session-Id' := SId,
                   'Accounting-Record-Type' := RT,
                   'Accounting-Record-Number' := 3 = RN}],
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
request(['ACR' | #{'Session-Id' := SId,
                   'Accounting-Record-Type' := RT,
                   'Accounting-Record-Number' := 1 = RN}],
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ACA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'Accounting-Record-Type', RT},
                    {'Accounting-Record-Number', RN}]};

%% send_protocol_error
request(['ACR' | #{'Accounting-Record-Number' := 4}],
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    %% Include a DOIC AVP that will be encoded/decoded because of
    %% avp_dictionaries config.
    OLR = #{'OC-Sequence-Number' => 1,
            'OC-Report-Type' => 0,  %% HOST_REPORT
            'OC-Reduction-Percentage' => [25],
            'OC-Validity-Duration' => [60],
            'AVP' => [{'OC-Supported-Features', []}]},
    %% Include a NAS Failed-AVP AVP that will only be decoded under
    %% that application. Encode as 'AVP' since RFC 3588 doesn't list
    %% Failed-AVP in the answer-message grammar while RFC 6733 does.
    NP = #diameter_avp{data = {nas4005, 'NAS-Port', 44}},
    FR = #diameter_avp{name = 'Firmware-Revision', value = 12}, %% M=0
    AP = #diameter_avp{name = 'Auth-Grace-Period', value = 13}, %% M=1
    Failed = #diameter_avp{data = {diameter_gen_base_rfc3588,
                                   'Failed-AVP',
                                   [{'AVP', [NP,FR,AP]}]}},
    Ans = ['answer-message', {'Result-Code', ?TOO_BUSY},
                             {'Origin-Host', OH},
                             {'Origin-Realm', OR},
                             {'AVP', [{'OC-OLR', OLR}, Failed]}],
    {reply, Ans};

%% send_proxy_info
request(['ASR' | #{'Proxy-Info' := _}],
        _) ->
    {protocol_error, 3999};

request(['ASR' | #{'Session-Id' := SId} = Avps],
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ASA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'AVP', maps:get('AVP', Avps, [])}]};

%% send_invalid_reject
request(['STR' | #{'Termination-Cause' := ?USER_MOVED}],
        _Caps) ->
    {protocol_error, ?TOO_BUSY};

%% send_noreply
request(['STR' | #{'Termination-Cause' := T}],
        _Caps)
  when T /= ?LOGOUT ->
    discard;

%% send_destination_5
request(['STR' | #{'Destination-Realm' := R}],
        #diameter_caps{origin_realm = {OR, _}})
  when R /= undefined, R /= OR ->
    {protocol_error, ?REALM_NOT_SERVED};

%% send_destination_6
request(['STR' | #{'Destination-Host' := [H]}],
        #diameter_caps{origin_host = {OH, _}})
  when H /= OH ->
    {protocol_error, ?UNABLE_TO_DELIVER};

request(['STR' | #{'Session-Id' := SId}],
        #diameter_caps{origin_host  = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['STA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR}]};

%% send_error/send_timeout
request(['RAR' | #{}], _Caps) ->
    receive after 2000 -> {protocol_error, ?TOO_BUSY} end.

%% message/3
%%
%% Limit the number of messages received. More can be received if read
%% in the same packet.

message(recv = D, {[_], Bin}, N) ->
    message(D, Bin, N);
message(Dir, #diameter_packet{bin = Bin}, N) ->
    message(Dir, Bin, N);

%% incoming request
message(recv, <<_:32, 1:1, _/bits>> = Bin, N) ->
    [Bin, N < 16, fun ?MODULE:message/3, N+1];

%% incoming answer
message(recv, Bin, _) ->
    [Bin];

%% outgoing
message(send, Bin, _) ->
    [Bin];

%% sent request
message(ack, <<_:32, 1:1, _/bits>>, _) ->
    [];

%% sent answer or discarded request
message(ack, _, N) ->
    [N =< 16, fun ?MODULE:message/3, N-1].

%% ------------------------------------------------------------------------

compile_and_load() ->
    try
        Path = hd([P || H <- [[here(), ".."], [code:lib_dir(diameter)]],
                        P <- [filename:join(H ++ ["examples",
                                                  "dict",
                                                  "rfc4005_nas.dia"])],
                        {ok, _} <- [file:read_file_info(P)]]),
        {ok, [Forms]}
            = diameter_make:codec(Path, [return,
                                      forms,
                                   {name, "nas4005"},
                                {prefix, "nas"},
                             {inherits, "common/diameter_gen_base_rfc3588"}]),
        {ok, nas4005, Bin, []} = compile:forms(Forms, [debug_info, return]),
        {module, nas4005} = code:load_binary(nas4005, "nas4005", Bin),
        true
    catch
        E:R:Stack ->
            {E, R, Stack}
    end.

here() ->
    filename:dirname(code:which(?MODULE)).
