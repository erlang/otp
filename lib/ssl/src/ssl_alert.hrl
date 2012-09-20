%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the SSL-alert protocol
%% see RFC 2246
%%----------------------------------------------------------------------

-ifndef(ssl_alert).
-define(ssl_alert, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alert protocol - RFC 2246 section 7.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% AlertLevel
-define(WARNING, 1).
-define(FATAL, 2).

%% {AlertDescription 
%% enum {
%%       close_notify(0),
%%       unexpected_message(10),
%%       bad_record_mac(20),
%%       decryption_failed(21),
%%       record_overflow(22),
%%       decompression_failure(30),
%%       handshake_failure(40),
%%       no_certificate_RESERVED(41), %% Only sslv3
%%       bad_certificate(42),
%%       unsupported_certificate(43),
%%       certificate_revoked(44),
%%       certificate_expired(45),
   %%       certificate_unknown(46),
%%       illegal_parameter(47),
%%       unknown_ca(48),
%%       access_denied(49),
%%       decode_error(50),
%%       decrypt_error(51),
%%       export_restriction(60),
%%       protocol_version(70),
%%       insufficient_security(71),
%%       internal_error(80),
%%       user_canceled(90),
%%       no_renegotiation(100),
%%       unknown_psk_identity(115),
%%           (255)
%%       } AlertDescription;

-define(CLOSE_NOTIFY, 0).
-define(UNEXPECTED_MESSAGE, 10).
-define(BAD_RECORD_MAC, 20).
-define(DECRYPTION_FAILED, 21).
-define(RECORD_OVERFLOW, 22).
-define(DECOMPRESSION_FAILURE, 30).
-define(HANDSHAKE_FAILURE, 40).
-define(NO_CERTIFICATE_RESERVED, 41).
-define(BAD_CERTIFICATE, 42).
-define(UNSUPPORTED_CERTIFICATE, 43).
-define(CERTIFICATE_REVOKED, 44).
-define(CERTIFICATE_EXPIRED, 45).
-define(CERTIFICATE_UNKNOWN, 46).
-define(ILLEGAL_PARAMETER, 47).
-define(UNKNOWN_CA, 48).
-define(ACCESS_DENIED, 49).
-define(DECODE_ERROR, 50).
-define(DECRYPT_ERROR, 51).
-define(EXPORT_RESTRICTION, 60).
-define(PROTOCOL_VERSION, 70).
-define(INSUFFICIENT_SECURITY, 71).
-define(INTERNAL_ERROR, 80).
-define(USER_CANCELED, 90).
-define(NO_RENEGOTIATION, 100).
-define(UNKNOWN_PSK_IDENTITY, 115).

-define(ALERT_REC(Level,Desc), #alert{level=Level,description=Desc,where={?FILE, ?LINE}}).

%% Alert
-record(alert, {
	  level,
	  description,
	  where = {?FILE, ?LINE}
	 }).
-endif. % -ifdef(ssl_alert).
