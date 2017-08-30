%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the DTLS-handshake protocol
%% that differs from TLS see RFC 6347 
%%----------------------------------------------------------------------
-ifndef(dtls_handshake).
-define(dtls_handshake, true).

-include("ssl_handshake.hrl"). %% Common TLS and DTLS records and Constantes

-define(HELLO_VERIFY_REQUEST, 3).
-define(HELLO_VERIFY_REQUEST_VERSION, {254, 255}).

-record(client_hello, {
	  client_version,
	  random,             
	  session_id,          % opaque SessionID<0..32>
	  cookie,              % opaque<2..2^16-1>
	  cipher_suites,       % cipher_suites<2..2^16-1>
	  compression_methods, % compression_methods<1..2^8-1>,
	  %% Extensions
	  extensions
	 }).

-record(hello_verify_request, {
	  protocol_version,
	  cookie
	 }).

-record(handshake_fragment, {
	  type,
	  length,
	  message_seq,               
	  fragment_offset,           
	  fragment_length,
	  fragment
	 }).

-endif. % -ifdef(dtls_handshake).
