%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(ssl_alert_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-include_lib("ssl/src/ssl_alert.hrl").

%% Common test
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([alerts/0,
         alerts/1,
         alert_details/0,
         alert_details/1,
         alert_details_not_too_big/0,
         alert_details_not_too_big/1,
         bad_connect_response/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     alerts,
     alert_details,
     alert_details_not_too_big,
     bad_connect_response
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
            Config0
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).
init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
alerts() ->
    [{doc, "Test ssl_alert formatting code"}].
alerts(Config) when is_list(Config) ->
    Descriptions = [?CLOSE_NOTIFY, ?UNEXPECTED_MESSAGE, ?BAD_RECORD_MAC,
		    ?DECRYPTION_FAILED_RESERVED, ?RECORD_OVERFLOW,
		    ?HANDSHAKE_FAILURE, ?BAD_CERTIFICATE, ?UNSUPPORTED_CERTIFICATE,
		    ?CERTIFICATE_REVOKED,?CERTIFICATE_EXPIRED, ?CERTIFICATE_UNKNOWN,
		    ?ILLEGAL_PARAMETER, ?UNKNOWN_CA, ?ACCESS_DENIED, ?DECODE_ERROR,
		    ?DECRYPT_ERROR, ?EXPORT_RESTRICTION, ?PROTOCOL_VERSION, 
		    ?INSUFFICIENT_SECURITY, ?INTERNAL_ERROR, ?USER_CANCELED,
		    ?NO_RENEGOTIATION, ?UNSUPPORTED_EXTENSION, ?CERTIFICATE_UNOBTAINABLE,
		    ?UNRECOGNIZED_NAME, ?BAD_CERTIFICATE_STATUS_RESPONSE,
		    ?BAD_CERTIFICATE_HASH_VALUE, ?UNKNOWN_PSK_IDENTITY, 
		    255 %% Unsupported/unknown alert will result in a description too
		   ],
    Alerts = [?ALERT_REC(?WARNING, ?CLOSE_NOTIFY) | 
	      [?ALERT_REC(?FATAL, Desc) || Desc <- Descriptions]],
    lists:foreach(fun(Alert) ->
                          try 
                              ssl_alert:reason_code(Alert, server, "TLS", cipher),
                              ssl_alert:reason_code(Alert, client, "TLS", hello)
                          catch
			    C:E:T ->
                                  ct:fail({unexpected, {C, E, T}})
			end 
		  end, Alerts).
%%--------------------------------------------------------------------
alert_details() ->
    [{doc, "Test that ssl_alert:alert_txt/1 result contains extendend error description"}].
alert_details(Config) when is_list(Config) ->
    Unique = make_ref(),
    UniqueStr = lists:flatten(io_lib:format("~w", [Unique])),
    Alert = ?ALERT_REC(?WARNING, ?INTERNAL_ERROR, Unique),
    {tls_alert, {_, Txt}} = ssl_alert:reason_code(Alert#alert{role=server}, server, "TLS", cipher),
    case string:str(Txt, UniqueStr) of
        0 ->
            ct:fail(error_details_missing);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
alert_details_not_too_big() ->
    [{doc, "Test that ssl_alert:alert_txt/1 limits printed depth of extended error description"}].
alert_details_not_too_big(Config) when is_list(Config) ->
    Reason = ssl:cipher_suites(all, 'tlsv1.2'),
    ReasonText = lists:flatten(io_lib:format("~p", [Reason])),
    Alert = ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason),
    PrefixLen = 
        length("TLS server: In state cipher at ssl_handshake.erl:1710 generated SERVER ALERT: Fatal - Handshake Failure"),
    {tls_alert, {_, Txt}} = ssl_alert:reason_code(Alert#alert{role=server, where = #{file => "ssl_handshake.erl", 
                                                                                     line => 1710}}, server, "TLS", cipher),
    case byte_size(term_to_binary(Txt)) < (byte_size(term_to_binary(ReasonText)) - PrefixLen) of
        true ->
            ?CT_LOG("~s", [Txt]);
        false ->
            ct:fail(ssl_alert_text_too_big)
    end.

%%--------------------------------------------------------------------
bad_connect_response(_Config) ->
    Me = self(),
    spawn_link(fun() -> echo_server_init(Me) end),
    Port = receive {port, P} -> P end,
    application:ensure_all_started(ssl),
    ok = check_response(catch ssl:connect("localhost", Port, [{versions, ['tlsv1.3']},
                                                              {verify, verify_none}])),
    ok = check_response(catch ssl:connect("localhost", Port, [{versions, ['tlsv1.2']},
                                                              {verify, verify_none}])),
    ok = check_response(catch ssl:connect("localhost", Port, [{versions, ['tlsv1.1']},
                                                              {verify, verify_none}])),
    ok.

check_response({error, {tls_alert, {unexpected_message, _}}}) ->
    ok;
check_response({error, {options, {insufficient_crypto_support,_}}}) ->
    ok;
check_response(What) ->
    ?CT_PAL("RES: ~p~n", [What]),
    What.

echo_server_init(Tester) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, true}, binary]),
    {ok, Port} = inet:port(Listen),
    Tester ! {port, Port},
    {ok, Socket} = gen_tcp:accept(Listen),
    echo_server(Socket, Listen).

echo_server(Socket, Listen) ->
    receive
        {tcp, Socket, Bin} when is_binary(Bin) ->
            gen_tcp:send(Socket, Bin),
            echo_server(Socket, Listen);
        {tcp_closed, Socket} ->
            {ok, New} = gen_tcp:accept(Listen),
            echo_server(New, Listen);
        Msg ->
            ?CT_PAL("Server: ~p~n", [Msg]),
            echo_server(Socket, Listen)
    end.


