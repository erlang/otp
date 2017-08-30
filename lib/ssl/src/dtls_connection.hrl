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
	  dtls_packets = [],              %%::[binary()],  % Not yet handled decode ssl/tls packets.
          dtls_record_buffer = <<>>,      %%:: binary(),   % Buffer of incomplete records
	  dtls_fragment_state,            %%:: [],         % DTLS fragments
          dtls_handshake_buffer = <<>>,   %%:: binary(),   % Buffer of incomplete handshakes
	  dtls_cipher_texts = [],         %%:: [binary()],
	  dtls_cipher_texts_next          %%:: [binary()]  % Received for Epoch not yet active
	 }).

-record(flight, {
	  last_retransmit,
	  last_read_seq,
	  msl_timer,
	  state,
	  buffer        % buffer of not yet ACKed TLS records
	 }).

-endif. % -ifdef(dtls_connection).
