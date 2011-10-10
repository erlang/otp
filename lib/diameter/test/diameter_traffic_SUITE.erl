%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
         send_arbitrary/1,
         send_unknown/1,
         send_unknown_mandatory/1,
         send_noreply/1,
         send_unsupported/1,
         send_unsupported_app/1,
         send_error_bit/1,
         send_unsupported_version/1,
         send_long/1,
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
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5, pick_peer/6,
         prepare_request/4, prepare_request/5,
         prepare_retransmit/4,
         handle_answer/5, handle_answer/6,
         handle_error/5,
         handle_request/3]).

-ifdef(DIAMETER_CT).
-include("diameter_gen_base_rfc3588.hrl").
-else.
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-endif.

-include_lib("diameter/include/diameter.hrl").
-include("diameter_ct.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).

-define(APP_ALIAS, base).
-define(EXTRA, an_extra_argument).
-define(ENCODINGS, [list, record]).

-define(DICT,   ?DIAMETER_DICT_COMMON).
-define(APP_ID, ?DIAMETER_APP_ID_COMMON).

%% Config for diameter:start_service/2.
-define(SERVICE(Name),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Acct-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {application, [{alias, ?APP_ALIAS},
                        {dictionary, ?DIAMETER_DICT_COMMON},
                        {module, ?MODULE},
                        {answer_errors, callback}]}]).

%% Config for diameter:add_transport/2. In the listening case, listen
%% on a free port that we then lookup using the implementation detail
%% that diameter_tcp registers the port with diameter_reg.
-define(CONNECT(PortNr),
        {connect, [{transport_module, diameter_tcp},
                   {transport_config, [{raddr, ?ADDR},
                                       {rport, PortNr},
                                       {ip, ?ADDR},
                                       {port, 0}]}]}).
-define(LISTEN,
        {listen, [{transport_module, diameter_tcp},
                  {transport_config, [{ip, ?ADDR}, {port, 0}]}]}).

-define(SUCCESS,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_SUCCESS').
-define(COMMAND_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_COMMAND_UNSUPPORTED').
-define(TOO_BUSY,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_TOO_BUSY').
-define(APPLICATION_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_APPLICATION_UNSUPPORTED').
-define(INVALID_HDR_BITS,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_INVALID_HDR_BITS').
-define(AVP_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_AVP_UNSUPPORTED').
-define(UNSUPPORTED_VERSION,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_UNSUPPORTED_VERSION').
-define(REALM_NOT_SERVED,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_REALM_NOT_SERVED').
-define(UNABLE_TO_DELIVER,
        ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_UNABLE_TO_DELIVER').

-define(EVENT_RECORD,
        ?'DIAMETER_BASE_ACCOUNTING-RECORD-TYPE_EVENT_RECORD').
-define(AUTHORIZE_ONLY,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').
-define(AUTHORIZE_AUTHENTICATE,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE').

-define(LOGOUT,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_DIAMETER_LOGOUT').
-define(BAD_ANSWER,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_DIAMETER_BAD_ANSWER').

-define(A, list_to_atom).
-define(L, atom_to_list).
-define(P(N), ?A("p_" ++ ?L(N))).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [start, start_services, add_transports, result_codes
     | [{group, N} || {N, _, _} <- groups()]]
        ++ [remove_transports, stop_services, stop].

groups() ->
    Ts = tc(),
    [{grp(E,P), P, Ts} || E <- ?ENCODINGS, P <- [[], [parallel]]].

grp(E, []) ->
    E;
grp(E, [parallel]) ->
    ?P(E).

init_per_group(Name, Config) ->
    E = case ?L(Name) of
            "p_" ++ Rest ->
                ?A(Rest);
            _ ->
                Name
        end,
    [{encode, E} | Config].

end_per_group(_, _) ->
    ok.

init_per_testcase(Name, Config) ->
    [{testcase, Name} | Config].

end_per_testcase(_, _) ->
    ok.

%% Testcases to run when services are started and connections
%% established.
tc() ->
    [send_ok,
     send_arbitrary,
     send_unknown,
     send_unknown_mandatory,
     send_noreply,
     send_unsupported,
     send_unsupported_app,
     send_error_bit,
     send_unsupported_version,
     send_long,
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

portnr() ->
    portnr(20).

portnr(N)
  when 0 < N ->
    case diameter_reg:match({diameter_tcp, listener, '_'}) of
        [{T, _Pid}] ->
            {_, _, {_LRef, {_Addr, LSock}}} = T,
            {ok, PortNr} = inet:port(LSock),
            PortNr;
        [] ->
            receive after 50 -> ok end,
            portnr(N-1)
    end.

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER)),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT)).

add_transports(Config) ->
    {ok, LRef} = diameter:add_transport(?SERVER, ?LISTEN),
    true = diameter:subscribe(?CLIENT),
    {ok, CRef} = diameter:add_transport(?CLIENT, ?CONNECT(portnr())),
    {up, CRef, _Peer, _Cfg, #diameter_packet{}}
        = receive #diameter_event{service = ?CLIENT, info = I} -> I
          after 2000 -> false
          end,
    true = diameter:unsubscribe(?CLIENT),
    ?util:write_priv(Config, "transport", {LRef, CRef}).

%% Remove the client transport and expect the server transport to
%% go down.
remove_transports(Config) ->
    {LRef, CRef} = ?util:read_priv(Config, "transport"),
    true = diameter:subscribe(?SERVER),
    ok = diameter:remove_transport(?CLIENT, CRef),
    {down, LRef, _, _}
        = receive #diameter_event{service = ?SERVER, info = I} -> I
          after 2000 -> false
          end.

stop_services(_Config) ->
    ok = diameter:stop_service(?CLIENT),
    ok = diameter:stop_service(?SERVER).

stop(_Config) ->
    ok = diameter:stop().

%% ===========================================================================

%% Ensure that result codes have the expected values.
result_codes(_Config) ->
    {2001, 3001, 3002, 3003, 3004, 3007, 3008, 5001, 5011}
        = {?SUCCESS,
           ?COMMAND_UNSUPPORTED,
           ?UNABLE_TO_DELIVER,
           ?REALM_NOT_SERVED,
           ?TOO_BUSY,
           ?APPLICATION_UNSUPPORTED,
           ?INVALID_HDR_BITS,
           ?AVP_UNSUPPORTED,
           ?UNSUPPORTED_VERSION}.

%% Send an ACR and expect success.
send_ok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 1}],
    #diameter_base_ACA{'Result-Code' = ?SUCCESS}
        = call(Config, Req).

%% Send an ASR with an arbitrary AVP and expect success and the same
%% AVP in the reply.
send_arbitrary(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{name = 'Class', value = "XXX"}]}],
    #diameter_base_ASA{'Result-Code' = ?SUCCESS,
                       'AVP' = Avps}
        = call(Config, Req),
    [#diameter_avp{name = 'Class',
                   value = "XXX"}]
        = Avps.

%% Send an unknown AVP (to some client) and check that it comes back.
send_unknown(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = false,
                                         data = <<17>>}]}],
    #diameter_base_ASA{'Result-Code' = ?SUCCESS,
                       'AVP' = Avps}
        = call(Config, Req),
    [#diameter_avp{code = 999,
                   is_mandatory = false,
                   data = <<17>>}]
        = Avps.

%% Ditto but set the M flag.
send_unknown_mandatory(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = true,
                                         data = <<17>>}]}],
    #diameter_base_ASA{'Result-Code' = ?AVP_UNSUPPORTED,
                       'Failed-AVP' = Failed}
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = Avps}] = Failed,
    [#diameter_avp{code = 999,
                   is_mandatory = true,
                   data = <<17>>}]
        = Avps.

%% Send an STR that the server ignores.
send_noreply(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    {error, timeout} = call(Config, Req).

%% Send an unsupported command and expect 3001.
send_unsupported(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    #'diameter_base_answer-message'{'Result-Code' = ?COMMAND_UNSUPPORTED}
        = call(Config, Req).

%% Send an unsupported command and expect 3007.
send_unsupported_app(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    #'diameter_base_answer-message'{'Result-Code' = ?APPLICATION_UNSUPPORTED}
        = call(Config, Req).

%% Send a request with the E bit set and expect 3008.
send_error_bit(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    #'diameter_base_answer-message'{'Result-Code' = ?INVALID_HDR_BITS}
        = call(Config, Req).

%% Send a bad version and check that we get 5011.
send_unsupported_version(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    #diameter_base_STA{'Result-Code' = ?UNSUPPORTED_VERSION}
        = call(Config, Req).

%% Send something long that will be fragmented by TCP.
send_long(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [lists:duplicate(1 bsl 20, $X)]}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(Config, Req).

%% Send something for which pick_peer finds no suitable peer.
send_nopeer(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{extra, [?EXTRA]}]).

%% Send something on an unconfigured application.
send_noapp(_Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = diameter:call(?CLIENT, unknown_alias, Req).

%% Send something that's discarded by prepare_request.
send_discard(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, unprepared} = call(Config, Req).

%% Send with a disjunctive filter.
send_any_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{filter, {any, []}}]).
send_any_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    #'diameter_base_answer-message'{'Result-Code' = ?UNABLE_TO_DELIVER}
        = call(Config, Req, [{filter, {any, [host, realm]}}]).

%% Send with a conjunctive filter.
send_all_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Realm = lists:foldr(fun(C,A) -> [C,A] end, [], ?REALM),
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(Config, Req, [{filter, {all, [{host, any},
                                             {realm, Realm}]}}]).
send_all_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Timeout before the server manages an answer.
send_timeout(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_ONLY}],
    {error, timeout} = call(Config, Req, [{timeout, 1000}]).

%% Explicitly answer with an answer-message and ensure that we
%% received the Session-Id.
send_error(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_AUTHENTICATE}],
    #'diameter_base_answer-message'{'Result-Code' = ?TOO_BUSY,
                                    'Session-Id' = SId}
        = call(Config, Req),
    undefined /= SId.

%% Send a request with the detached option and receive it as a message
%% from handle_answer instead.
send_detach(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Ref = make_ref(),
    ok = call(Config, Req, [{extra, [{self(), Ref}]}, detach]),
    #diameter_packet{msg = Rec, errors = []}
        = receive {Ref, T} -> T after 2000 -> false end,
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = Rec.

%% Send a request which can't be encoded and expect {error, encode}.
send_encode_error(Config) ->
    {error, encode} = call(Config, ['STR']).  %% No Termination-Cause

%% Send with filtering and expect success.
send_destination_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, ?REALM)]}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send with filtering on and expect failure when specifying an
%% unknown host or realm.
send_destination_3(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_4(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send without filtering and expect an error answer when specifying
%% an unknown host or realm.
send_destination_5(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    #'diameter_base_answer-message'{'Result-Code' = ?REALM_NOT_SERVED}
        = call(Config, Req).
send_destination_6(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    #'diameter_base_answer-message'{'Result-Code' = ?UNABLE_TO_DELIVER}
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
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
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
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = send_multiple_filters(Config, [{eval, E} || E <- [E1,E2,E3,E4]]).

send_multiple_filters(Config, Fs) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    call(Config, Req, [{filter, F} || F <- Fs]).

%% Ensure that we can pass a request in any form to diameter:call/4,
%% only the return value from the prepare_request callback being
%% significant.
send_anything(Config) ->    
    #diameter_base_STA{'Result-Code' = ?SUCCESS}
        = call(Config, anything).
    
%% ===========================================================================

call(Config, Req) ->
    call(Config, Req, []).

call(Config, Req, Opts) ->
    Name = proplists:get_value(testcase, Config),
    Enc = proplists:get_value(encode, Config),
    diameter:call(?CLIENT,
                  ?APP_ALIAS,
                  msg(Req, Enc),
                  [{extra, [Name]} | Opts]).

msg([_|_] = L, list) ->
    L;
msg([H|T], record) ->
    ?DICT:'#new-'(?DICT:msg2rec(H), T);
msg(T, _) ->
    T.

%% Set only values that aren't already.
set([H|T], Vs) ->
    [H | Vs ++ T];
set(Rec, Vs) ->
    lists:foldl(fun({F,_} = FV, A) -> set(?DICT:'#get-'(F, A), FV, A) end,
                Rec,
                Vs).

set(E, FV, Rec)
  when E == undefined;
       E == [] ->
    ?DICT:'#set-'(FV, Rec);
set(_, _, Rec) ->
    Rec.

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/5/6

pick_peer([Peer], _, ?CLIENT, _State, Name)
  when Name /= send_detach ->
    {ok, Peer}.

pick_peer([_Peer], _, ?CLIENT, _State, send_nopeer, ?EXTRA) ->
    false;

pick_peer([Peer], _, ?CLIENT, _State, send_detach, {_,_}) ->
    {ok, Peer}.

%% prepare_request/4/5

prepare_request(_Pkt, ?CLIENT, {_Ref, _Caps}, send_discard) ->
    {discard, unprepared};

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, Name) ->
    {send, prepare(Pkt, Caps, Name)}.

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, send_detach, _) ->
    {send, prepare(Pkt, Caps)}.

prepare(Pkt, Caps, send_unsupported) ->
    Req = prepare(Pkt, Caps),
    #diameter_packet{bin = <<H:5/binary, _CmdCode:3/binary, T/binary>>}
        = E
        = diameter_codec:encode(?DICT, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, 42:24/integer, T/binary>>};

prepare(Pkt, Caps, send_unsupported_app) ->
    Req = prepare(Pkt, Caps),
    #diameter_packet{bin = <<H:8/binary, _ApplId:4/binary, T/binary>>}
        = E
        = diameter_codec:encode(?DICT, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, 42:32/integer, T/binary>>};

prepare(Pkt, Caps, send_error_bit) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{is_error = true},
                        msg = prepare(Pkt, Caps)};

prepare(Pkt, Caps, send_unsupported_version) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{version = 42},
                        msg = prepare(Pkt, Caps)};

prepare(Pkt, Caps, send_anything) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    prepare(Pkt#diameter_packet{msg = Req}, Caps);

prepare(Pkt, Caps, _Name) ->
    prepare(Pkt, Caps).

prepare(#diameter_packet{msg = Req}, Caps)
  when is_record(Req, diameter_base_ACR);
       'ACR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,

    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR},
              {'Destination-Realm', DR}]);

prepare(#diameter_packet{msg = Req}, Caps)
  when is_record(Req, diameter_base_ASR);
       'ASR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR},
              {'Destination-Host',  DH},
              {'Destination-Realm', DR},
              {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps)
  when is_record(Req, diameter_base_STR);
       'STR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR},
              {'Destination-Realm', DR},
              {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps)
  when is_record(Req, diameter_base_RAR);
       'RAR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Req, [{'Session-Id', diameter:session_id(OH)},
              {'Origin-Host',  OH},
              {'Origin-Realm', OR},
              {'Destination-Host',  DH},
              {'Destination-Realm', DR},
              {'Auth-Application-Id', ?APP_ID}]).

%% prepare_retransmit/4

prepare_retransmit(_Pkt, false, _Peer, _Name) ->
    discard.

%% handle_answer/5/6

handle_answer(Pkt, Req, ?CLIENT, Peer, Name) ->
    answer(Pkt, Req, Peer, Name).

handle_answer(Pkt, _Req, ?CLIENT, _Peer, send_detach, {Pid, Ref}) ->
    Pid ! {Ref, Pkt}.

answer(#diameter_packet{msg = Rec, errors = []}, _Req, _Peer, _) ->
    Rec.

%% handle_error/5

handle_error(Reason, _Req, ?CLIENT, _Peer, _Name) ->
    {error, Reason}.

%% handle_request/3

%% Note that diameter will set Result-Code and Failed-AVPs if
%% #diameter_packet.errors is non-null.

handle_request(Pkt, ?SERVER, {_Ref, Caps}) ->
    request(Pkt, Caps).

request(#diameter_packet{msg
                         = #diameter_base_ACR{'Session-Id' = SId,
                                              'Accounting-Record-Type' = RT,
                                              'Accounting-Record-Number' = RN}},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ACA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'Accounting-Record-Type', RT},
                    {'Accounting-Record-Number', RN}]};

request(#diameter_packet{msg = #diameter_base_ASR{'Session-Id' = SId,
                                                  'AVP' = Avps}},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, #diameter_base_ASA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'AVP' = Avps}};

request(#diameter_packet{msg = #diameter_base_STR{'Termination-Cause' = T}},
        _Caps)
  when T /= ?LOGOUT ->
    discard;

request(#diameter_packet{msg = #diameter_base_STR{'Destination-Realm'= R}},
        #diameter_caps{origin_realm = {OR, _}})
  when R /= undefined, R /= OR ->
    {protocol_error, ?REALM_NOT_SERVED};

request(#diameter_packet{msg = #diameter_base_STR{'Destination-Host'= [H]}},
        #diameter_caps{origin_host = {OH, _}})
  when H /= OH ->
    {protocol_error, ?UNABLE_TO_DELIVER};

request(#diameter_packet{msg = #diameter_base_STR{'Session-Id' = SId}},
        #diameter_caps{origin_host  = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}};

request(#diameter_packet{msg = #diameter_base_RAR{}}, _Caps) ->
    receive after 2000 -> ok end,
    {protocol_error, ?TOO_BUSY}.
