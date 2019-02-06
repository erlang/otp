%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
%% Purpose: SSL/TLS specific state
%%----------------------------------------------------------------------

-ifndef(dtls_connection).
-define(dtls_connection, true).

-include("ssl_connection.hrl").

-record(protocol_buffers, {
          dtls_record_buffer = <<>>,      %% Buffer of incomplete records
	  dtls_handshake_next_seq = 0,
	  dtls_flight_last,
	  dtls_handshake_next_fragments = [], %% Fragments of the next handshake message
	  dtls_handshake_later_fragments = [], %% Fragments of handsake messages come after the one in next buffer
	  dtls_cipher_texts = []         %%:: [binary()],
	 }).

-define(INITIAL_RETRANSMIT_TIMEOUT, 1000). %1 sec

-endif. % -ifdef(dtls_connection).
