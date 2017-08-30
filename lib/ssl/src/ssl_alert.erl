%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles an ssl connection, e.i. both the setup
%% e.i. SSL-Handshake, SSL-Alert and SSL-Cipher protocols and delivering
%% data to the application. All data on the connectinon is received and 
%% sent according to the SSL-record protocol.  
%% %%----------------------------------------------------------------------

-module(ssl_alert).

-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_internal.hrl").

-export([decode/1, alert_txt/1, reason_code/2]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec decode(binary()) -> [#alert{}] | #alert{}.
%%
%% Description: Decode alert(s), will return a singel own alert if peer
%% sends garbage or too many warning alerts.
%%--------------------------------------------------------------------
decode(Bin) ->
    decode(Bin, [], 0).

%%--------------------------------------------------------------------
-spec reason_code(#alert{}, client | server) -> closed | {essl, string()}.
%%
%% Description: Returns the error reason that will be returned to the
%% user.
%%--------------------------------------------------------------------

reason_code(#alert{description = ?CLOSE_NOTIFY}, _) ->
    closed;
reason_code(#alert{description = Description}, _) ->
    {tls_alert, description_txt(Description)}.

%%--------------------------------------------------------------------
-spec alert_txt(#alert{}) -> string().
%%
%% Description: Returns the error string for given alert.
%%--------------------------------------------------------------------
alert_txt(#alert{level = Level, description = Description, where = {Mod,Line}, reason = undefined}) ->
    Mod ++ ":" ++ integer_to_list(Line) ++ ":" ++ 
        level_txt(Level) ++" "++ description_txt(Description);
alert_txt(#alert{reason = Reason} = Alert) ->
    BaseTxt = alert_txt(Alert#alert{reason = undefined}),
    FormatDepth = 9, % Some limit on printed representation of an error
    ReasonTxt = lists:flatten(io_lib:format("~P", [Reason, FormatDepth])),
    BaseTxt ++ " - " ++ ReasonTxt.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% It is very unlikely that an correct implementation will send more than one alert at the time
%% So it there is more than 10 warning alerts we consider it an error
decode(<<?BYTE(Level), ?BYTE(_), _/binary>>, _, N) when Level == ?WARNING, N > ?MAX_ALERTS ->
    ?ALERT_REC(?FATAL, ?DECODE_ERROR, too_many_remote_alerts);
decode(<<?BYTE(Level), ?BYTE(Description), Rest/binary>>, Acc, N) when Level == ?WARNING ->
    Alert = ?ALERT_REC(Level, Description),
    decode(Rest, [Alert | Acc], N + 1);
decode(<<?BYTE(Level), ?BYTE(Description), _Rest/binary>>, Acc, _) when Level == ?FATAL->
    Alert = ?ALERT_REC(Level, Description),
    lists:reverse([Alert | Acc]); %% No need to decode rest fatal alert will end the connection
decode(<<?BYTE(_Level), _/binary>>, _, _) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, failed_to_decode_remote_alert);
decode(<<>>, Acc, _) ->
    lists:reverse(Acc, []).

level_txt(?WARNING) ->
    "Warning:";
level_txt(?FATAL) ->
    "Fatal error:".

description_txt(?CLOSE_NOTIFY) ->
    "close notify";
description_txt(?UNEXPECTED_MESSAGE) ->
    "unexpected message";
description_txt(?BAD_RECORD_MAC) ->
    "bad record mac";
description_txt(?DECRYPTION_FAILED) ->
    "decryption failed";
description_txt(?RECORD_OVERFLOW) ->
    "record overflow";
description_txt(?DECOMPRESSION_FAILURE) ->
    "decompression failure";
description_txt(?HANDSHAKE_FAILURE) ->
    "handshake failure";
description_txt(?NO_CERTIFICATE_RESERVED) ->
    "No certificate reserved";
description_txt(?BAD_CERTIFICATE) ->
    "bad certificate";
description_txt(?UNSUPPORTED_CERTIFICATE) ->
    "unsupported certificate";
description_txt(?CERTIFICATE_REVOKED) ->
    "certificate revoked";
description_txt(?CERTIFICATE_EXPIRED) ->
    "certificate expired";
description_txt(?CERTIFICATE_UNKNOWN) ->
    "certificate unknown";
description_txt(?ILLEGAL_PARAMETER) ->
    "illegal parameter";
description_txt(?UNKNOWN_CA) ->
    "unknown ca";
description_txt(?ACCESS_DENIED) ->
    "access denied";
description_txt(?DECODE_ERROR) ->
    "decode error";
description_txt(?DECRYPT_ERROR) ->
    "decrypt error";
description_txt(?EXPORT_RESTRICTION) ->
    "export restriction";
description_txt(?PROTOCOL_VERSION) ->
    "protocol version";
description_txt(?INSUFFICIENT_SECURITY) ->
    "insufficient security";
description_txt(?INTERNAL_ERROR) ->
    "internal error";
description_txt(?USER_CANCELED) ->
    "user canceled";
description_txt(?NO_RENEGOTIATION) ->
    "no renegotiation";
description_txt(?UNSUPPORTED_EXTENSION) ->
    "unsupported extension";
description_txt(?CERTIFICATE_UNOBTAINABLE) ->
    "certificate unobtainable";
description_txt(?UNRECOGNISED_NAME) ->
    "unrecognised name";
description_txt(?BAD_CERTIFICATE_STATUS_RESPONSE) ->
    "bad certificate status response";
description_txt(?BAD_CERTIFICATE_HASH_VALUE) ->
    "bad certificate hash value";
description_txt(?UNKNOWN_PSK_IDENTITY) ->
    "unknown psk identity";
description_txt(?INAPPROPRIATE_FALLBACK) ->
    "inappropriate fallback";
description_txt(?NO_APPLICATION_PROTOCOL) ->
    "no application protocol";
description_txt(Enum) ->
    lists:flatten(io_lib:format("unsupported/unknown alert: ~p", [Enum])).
