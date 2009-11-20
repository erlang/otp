%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles an ssl connection, e.i. both the setup
%% e.i. SSL-Handshake, SSL-Alert and SSL-Cipher protocols and delivering
%% data to the application. All data on the connectinon is received and 
%% sent according to the SSL-record protocol.  
%% %%----------------------------------------------------------------------

-module(ssl_alert).

-include("ssl_alert.hrl").
-include("ssl_record.hrl").

-export([alert_txt/1, reason_code/2]).

reason_code(#alert{description = ?CLOSE_NOTIFY}, _) ->
    closed;
reason_code(#alert{description = ?HANDSHAKE_FAILURE}, client) ->
    esslconnect;
reason_code(#alert{description = ?HANDSHAKE_FAILURE}, server) ->
    esslaccept;
reason_code(#alert{description = ?CERTIFICATE_EXPIRED}, _) ->
    epeercertexpired;
reason_code(#alert{level = ?FATAL}, _) ->
    esslerrssl;
reason_code(#alert{description = Description}, _) ->
    description_txt(Description).

alert_txt(#alert{level = Level, description = Description, where = {Mod,Line}}) ->
    Mod ++ ":" ++ integer_to_list(Line) ++ ":" ++ 
	level_txt(Level) ++" "++ description_txt(Description).

level_txt(?WARNING) ->
    "Warning:";
level_txt(?FATAL) ->
    "Fatal error:".

description_txt(?CLOSE_NOTIFY) ->
    "close_notify";
description_txt(?UNEXPECTED_MESSAGE) ->
    "unexpected_message";
description_txt(?BAD_RECORD_MAC) ->
    "bad_record_mac";
description_txt(?DECRYPTION_FAILED) ->
    "decryption_failed";
description_txt(?RECORD_OVERFLOW) ->
    "record_overflow";
description_txt(?DECOMPRESSION_FAILURE) ->
    "decompression_failure";
description_txt(?HANDSHAKE_FAILURE) ->
    "handshake_failure";
description_txt(?BAD_CERTIFICATE) ->
    "bad_certificate";
description_txt(?UNSUPPORTED_CERTIFICATE) ->
    "unsupported_certificate";
description_txt(?CERTIFICATE_REVOKED) ->
    "certificate_revoked";
description_txt(?CERTIFICATE_EXPIRED) ->
    "certificate_expired";
description_txt(?CERTIFICATE_UNKNOWN) ->
    "certificate_unknown";
description_txt(?ILLEGAL_PARAMETER) ->
    "illegal_parameter";
description_txt(?UNKNOWN_CA) ->
    "unknown_ca";
description_txt(?ACCESS_DENIED) ->
    "access_denied";
description_txt(?DECODE_ERROR) ->
    "decode_error";
description_txt(?DECRYPT_ERROR) ->
    "decrypt_error";
description_txt(?EXPORT_RESTRICTION) ->
    "export_restriction";
description_txt(?PROTOCOL_VERSION) ->
    "protocol_version";
description_txt(?INSUFFICIENT_SECURITY) ->
    "insufficient_security";
description_txt(?INTERNAL_ERROR) ->
    "internal_error";
description_txt(?USER_CANCELED) ->
    "user_canceled";
description_txt(?NO_RENEGOTIATION) ->
    "no_renegotiation".





