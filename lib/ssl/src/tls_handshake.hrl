%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the TLS-handshake protocol
%% see RFC 5246. 
%%----------------------------------------------------------------------
-ifndef(tls_handshake).
-define(tls_handshake, true).

-include("ssl_handshake.hrl"). %% Common TLS and DTLS records and Constantes

-record(client_hello, {
	  client_version,
	  random,             
	  session_id,          % opaque SessionID<0..32>
	  cipher_suites,       % cipher_suites<2..2^16-1>
	  compression_methods, % compression_methods<1..2^8-1>,
	  %% Extensions
	  extensions
	 }).

-endif. % -ifdef(tls_handshake).
