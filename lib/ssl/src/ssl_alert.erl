%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

-export([decode/1, 
         own_alert_format/4, 
         alert_format/4,
         reason_code/4]).

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
-spec reason_code(#alert{}, client | server, ProtocolName::string(), StateName::atom()) ->
                         {tls_alert, {atom(), unicode:chardata()}} | closed.
%%
%% Description: Returns the error reason that will be returned to the
%% user.
%%--------------------------------------------------------------------

reason_code(#alert{description = ?CLOSE_NOTIFY}, _, _, _) ->
    closed;
reason_code(#alert{description = Description, role = Role} = Alert, Role, ProtocolName, StateName) ->
    {PrefixFmt, PrefixArgs} = alert_prefix_format(ProtocolName, Role, StateName),
    {Fmt, Args} = own_alert_format_depth(Alert),
    Txt = lists:flatten(io_lib:format(PrefixFmt ++ Fmt, PrefixArgs ++ Args)),
    {tls_alert, {description_atom(Description), Txt}};
reason_code(#alert{description = Description} = Alert, Role, ProtocolName, StateName) ->
    {Fmt, Args} = alert_format(ProtocolName, Role, StateName, Alert),
    Txt = lists:flatten(io_lib:format(Fmt, Args)),
    {tls_alert, {description_atom(Description), Txt}}.

%%--------------------------------------------------------------------
-spec own_alert_format(string(), server | client, StateNam::atom(), #alert{}) -> {io:format(), list()}.
%%
%% Description: Generates alert text for log or string part of error return.
%%--------------------------------------------------------------------
own_alert_format(ProtocolName, Role, StateName, Alert) ->
    {PrfixFmt, PrefixArgs} = alert_prefix_format(ProtocolName, Role, StateName),
    {Fmt, Args} = own_alert_format(Alert),
    {PrfixFmt ++ Fmt, PrefixArgs ++ Args}.

%%--------------------------------------------------------------------
-spec alert_format(string(), server | client, StateNam::atom(), #alert{}) -> {io:format(), list()}.
%%
%% Description: Generates alert text for log or string part of error return.
%%--------------------------------------------------------------------
alert_format(ProtocolName, Role, StateName, Alert) ->
    {PrfixFmt, PrefixArgs} = alert_prefix_format(ProtocolName, Role, StateName),
    {Fmt, Args} = alert_format(Alert),
    {PrfixFmt ++ Fmt, PrefixArgs ++ Args}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
alert_prefix_format(ProtocolName, Role, StateName) ->
    {"~s ~p: In state ~p", [ProtocolName, Role, StateName]}.

own_alert_format(#alert{reason = Reason} = Alert) ->   
    Txt = own_alert_txt(Alert),
    case Reason of
        undefined ->            
            {" ~s\n", [Txt]};
        Reason ->
            {" ~s\n - ~p", [Txt, Reason]}
    end.
own_alert_format_depth(#alert{reason = Reason} = Alert) ->   
    Txt = own_alert_txt(Alert),
    case Reason of
        undefined ->            
            {" ~s\n", [Txt]};
        Reason ->
            {" ~s\n ~P", [Txt, Reason, ?DEPTH]}
    end.

own_alert_txt(#alert{level = Level, description = Description, where = #{line := Line, file := Mod}, role = Role}) ->
    "at " ++ Mod ++ ":" ++ integer_to_list(Line) ++ " generated " ++ string:uppercase(atom_to_list(Role)) ++ " ALERT: " ++
        level_txt(Level) ++ description_txt(Description).

alert_format(Alert) ->
    Txt = alert_txt(Alert),
    {" ~s\n ", [Txt]}.

alert_txt(#alert{level = Level, description = Description, role = Role}) ->
    "received " ++ string:uppercase(atom_to_list(Role)) ++ " ALERT: " ++
        level_txt(Level) ++ description_txt(Description).

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
description_txt(?INAPPROPRIATE_FALLBACK) ->
    "Inappropriate Fallback";
description_txt(?USER_CANCELED) ->
    "User Canceled";
description_txt(?NO_RENEGOTIATION) ->
    "No Renegotiation";
description_txt(?MISSING_EXTENSION) ->
    "Missing extension";
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
description_txt(?CERTIFICATE_REQUIRED) ->
    "Certificate required";
description_txt(?NO_APPLICATION_PROTOCOL) ->
    "No application protocol";
description_txt(Enum) ->
    lists:flatten(io_lib:format("unsupported/unknown alert: ~p", [Enum])).

description_atom(?CLOSE_NOTIFY) ->
    close_notify;
description_atom(?UNEXPECTED_MESSAGE) ->
    unexpected_message;
description_atom(?BAD_RECORD_MAC) ->
    bad_record_mac;
description_atom(?DECRYPTION_FAILED_RESERVED) ->
    decryption_failed_reserved;
description_atom(?RECORD_OVERFLOW) ->
    record_overflow;
description_atom(?DECOMPRESSION_FAILURE) ->
    decompression_failure;
description_atom(?HANDSHAKE_FAILURE) ->
    handshake_failure;
description_atom(?NO_CERTIFICATE_RESERVED) ->
    no_certificate_reserved;
description_atom(?BAD_CERTIFICATE) ->
    bad_certificate;
description_atom(?UNSUPPORTED_CERTIFICATE) ->
    unsupported_certificate;
description_atom(?CERTIFICATE_REVOKED) ->
    certificate_revoked;
description_atom(?CERTIFICATE_EXPIRED) ->
    certificate_expired;
description_atom(?CERTIFICATE_UNKNOWN) ->
    certificate_unknown;
description_atom(?ILLEGAL_PARAMETER) ->
    illegal_parameter;
description_atom(?UNKNOWN_CA) ->
    unknown_ca;
description_atom(?ACCESS_DENIED) ->
    access_denied;
description_atom(?DECODE_ERROR) ->
    decode_error;
description_atom(?DECRYPT_ERROR) ->
    decrypt_error;
description_atom(?EXPORT_RESTRICTION) ->
    export_restriction;
description_atom(?PROTOCOL_VERSION) ->
    protocol_version;
description_atom(?INSUFFICIENT_SECURITY) ->
    insufficient_security;
description_atom(?INTERNAL_ERROR) ->
    internal_error;
description_atom(?INAPPROPRIATE_FALLBACK) ->
    inappropriate_fallback;
description_atom(?USER_CANCELED) ->
    user_canceled;
description_atom(?NO_RENEGOTIATION) ->
    no_renegotiation;
description_atom(?MISSING_EXTENSION) ->
    missing_extension;
description_atom(?UNSUPPORTED_EXTENSION) ->
    unsupported_extension;
description_atom(?CERTIFICATE_UNOBTAINABLE) ->
    certificate_unobtainable;
description_atom(?UNRECOGNISED_NAME) ->
    unrecognised_name;
description_atom(?BAD_CERTIFICATE_STATUS_RESPONSE) ->
    bad_certificate_status_response;
description_atom(?BAD_CERTIFICATE_HASH_VALUE) ->
    bad_certificate_hash_value;
description_atom(?UNKNOWN_PSK_IDENTITY) ->
    unknown_psk_identity;
description_atom(?CERTIFICATE_REQUIRED) ->
    certificate_required;
description_atom(?NO_APPLICATION_PROTOCOL) ->
    no_application_protocol;
description_atom(_) ->
    'unsupported/unknown_alert'.
