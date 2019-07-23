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

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-include_lib("ssl/src/ssl_alert.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     alerts,
     alert_details,
     alert_details_not_too_big
    ].

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
alerts() ->
    [{doc, "Test ssl_alert:alert_txt/1"}].
alerts(Config) when is_list(Config) ->
    Descriptions = [?CLOSE_NOTIFY, ?UNEXPECTED_MESSAGE, ?BAD_RECORD_MAC,
		    ?DECRYPTION_FAILED_RESERVED, ?RECORD_OVERFLOW, ?DECOMPRESSION_FAILURE,
		    ?HANDSHAKE_FAILURE, ?BAD_CERTIFICATE, ?UNSUPPORTED_CERTIFICATE,
		    ?CERTIFICATE_REVOKED,?CERTIFICATE_EXPIRED, ?CERTIFICATE_UNKNOWN,
		    ?ILLEGAL_PARAMETER, ?UNKNOWN_CA, ?ACCESS_DENIED, ?DECODE_ERROR,
		    ?DECRYPT_ERROR, ?EXPORT_RESTRICTION, ?PROTOCOL_VERSION, 
		    ?INSUFFICIENT_SECURITY, ?INTERNAL_ERROR, ?USER_CANCELED,
		    ?NO_RENEGOTIATION, ?UNSUPPORTED_EXTENSION, ?CERTIFICATE_UNOBTAINABLE,
		    ?UNRECOGNISED_NAME, ?BAD_CERTIFICATE_STATUS_RESPONSE,
		    ?BAD_CERTIFICATE_HASH_VALUE, ?UNKNOWN_PSK_IDENTITY, 
		    255 %% Unsupported/unknow alert will result in a description too
		   ],
    Alerts = [?ALERT_REC(?WARNING, ?CLOSE_NOTIFY) | 
	      [?ALERT_REC(?FATAL, Desc) || Desc <- Descriptions]],
    lists:foreach(fun(Alert) ->
                          try ssl_alert:alert_txt(Alert)
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
    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY, Unique),
    case string:str(ssl_alert:alert_txt(Alert), UniqueStr) of
        0 ->
            ct:fail(error_details_missing);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
alert_details_not_too_big() ->
    [{doc, "Test that ssl_alert:alert_txt/1 limits printed depth of extended error description"}].
alert_details_not_too_big(Config) when is_list(Config) ->
    Reason = lists:duplicate(10, lists:duplicate(10, lists:duplicate(10, {some, data}))),
    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY, Reason),
    case length(ssl_alert:alert_txt(Alert)) < 1000 of
        true ->
            ok;
        false ->
            ct:fail(ssl_alert_text_too_big)
    end.
