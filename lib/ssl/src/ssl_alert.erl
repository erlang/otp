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

-export([decode/1, own_alert_txt/1, alert_txt/1, reason_code/2]).

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
-spec reason_code(#alert{}, client | server) ->
                         closed | {tls_alert, unicode:chardata()}.
%-spec reason_code(#alert{}, client | server) -> closed | {essl, string()}.
%%
%% Description: Returns the error reason that will be returned to the
%% user.
%%--------------------------------------------------------------------

reason_code(#alert{description = ?CLOSE_NOTIFY}, _) ->
    closed;
reason_code(#alert{description = Description}, _) ->
    {tls_alert, string:casefold(description_txt(Description))}.

%%--------------------------------------------------------------------
-spec own_alert_txt(#alert{}) -> string().
%%
%% Description: Returns the error string for given alert generated
%% by the erlang implementation.
%%--------------------------------------------------------------------
own_alert_txt(#alert{level = Level, description = Description, where = {Mod,Line}, reason = undefined, role = Role}) ->
    "at " ++ Mod ++ ":" ++ integer_to_list(Line) ++ " generated " ++ string:uppercase(atom_to_list(Role)) ++ " ALERT: " ++
        level_txt(Level) ++ description_txt(Description);
own_alert_txt(#alert{reason = Reason} = Alert) ->
    BaseTxt = own_alert_txt(Alert#alert{reason = undefined}),
    FormatDepth = 9, % Some limit on printed representation of an error
    ReasonTxt = lists:flatten(io_lib:format("~P", [Reason, FormatDepth])),
    BaseTxt ++ " - " ++ ReasonTxt.

%%--------------------------------------------------------------------
-spec alert_txt(#alert{}) -> string().
%%
%% Description: Returns the error string for given alert received from
%% the peer. 
%%--------------------------------------------------------------------
alert_txt(#alert{level = Level, description = Description, reason = undefined, role = Role}) ->
    "received " ++ string:uppercase(atom_to_list(Role)) ++ " ALERT: " ++
        level_txt(Level) ++ description_txt(Description);
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
    "Warning - ";
level_txt(?FATAL) ->
    "Fatal - ".

description_txt(?CLOSE_NOTIFY) ->
    "Close Notify";
description_txt(?UNEXPECTED_MESSAGE) ->
    "Unexpected Message";
description_txt(?BAD_RECORD_MAC) ->
    "Bad Record MAC";
description_txt(?DECRYPTION_FAILED_RESERVED) ->
    "Decryption Failed Reserved";
description_txt(?RECORD_OVERFLOW) ->
    "Record Overflow";
description_txt(?DECOMPRESSION_FAILURE) ->
    "Decompression Failure";
description_txt(?HANDSHAKE_FAILURE) ->
    "Handshake Failure";
description_txt(?NO_CERTIFICATE_RESERVED) ->
    "No Certificate Reserved";
description_txt(?BAD_CERTIFICATE) ->
    "Bad Certificate";
description_txt(?UNSUPPORTED_CERTIFICATE) ->
    "Unsupported Certificate";
description_txt(?CERTIFICATE_REVOKED) ->
    "Certificate Revoked";
description_txt(?CERTIFICATE_EXPIRED) ->
    "Certificate Expired";
description_txt(?CERTIFICATE_UNKNOWN) ->
    "Certificate Unknown";
description_txt(?ILLEGAL_PARAMETER) ->
    "Illegal Parameter";
description_txt(?UNKNOWN_CA) ->
    "Unknown CA";
description_txt(?ACCESS_DENIED) ->
    "Access Denied";
description_txt(?DECODE_ERROR) ->
    "Decode Error";
description_txt(?DECRYPT_ERROR) ->
    "Decrypt Error";
description_txt(?EXPORT_RESTRICTION) ->
    "Export Restriction";
description_txt(?PROTOCOL_VERSION) ->
    "Protocol Version";
description_txt(?INSUFFICIENT_SECURITY) ->
    "Insufficient Security";
description_txt(?INTERNAL_ERROR) ->
    "Internal Error";
description_txt(?USER_CANCELED) ->
    "User Canceled";
description_txt(?NO_RENEGOTIATION) ->
    "No Renegotiation";
description_txt(?UNSUPPORTED_EXTENSION) ->
    "Unsupported Extension";
description_txt(?CERTIFICATE_UNOBTAINABLE) ->
    "Certificate Unobtainable";
description_txt(?UNRECOGNISED_NAME) ->
    "Unrecognised Name";
description_txt(?BAD_CERTIFICATE_STATUS_RESPONSE) ->
    "Bad Certificate Status Response";
description_txt(?BAD_CERTIFICATE_HASH_VALUE) ->
    "Bad Certificate Hash Value";
description_txt(?UNKNOWN_PSK_IDENTITY) ->
    "Unknown Psk Identity";
description_txt(?INAPPROPRIATE_FALLBACK) ->
    "Inappropriate Fallback";
description_txt(?NO_APPLICATION_PROTOCOL) ->
    "No application protocol";
description_txt(Enum) ->
    lists:flatten(io_lib:format("unsupported/unknown alert: ~p", [Enum])).
