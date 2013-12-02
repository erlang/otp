%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
%% Purpose: SSL/TLS specific state
%%----------------------------------------------------------------------

-ifndef(dtls_connection).
-define(dtls_connection, true).

-include("ssl_connection.hrl").

-record(protocol_buffers, {
	  dtls_packets = []      ::[binary()],  % Not yet handled decode ssl/tls packets.
          dtls_record_buffer     :: binary(),   % Buffer of incomplete records
          dtls_handshake_buffer  :: binary(),   % Buffer of incomplete handshakes
	  dtls_cipher_texts      :: [binary()],
	  dtls_cipher_texts_next :: [binary()]  % Received for Epoch not yet active
	 }).

-record(flight, {
	  last_retransmit,
	  last_read_seq,
	  msl_timer,
	  flight_state,
	  flight_buffer,        % buffer of not yet ACKed TLS records
	 }).

-record(message_sequences, {
	  read = 0,
	  write = 0
	 }).

-endif. % -ifdef(dtls_connection).
