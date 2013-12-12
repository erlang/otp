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

-ifndef(tls_connection).
-define(tls_connection, true).

-include("ssl_connection.hrl").
-include("tls_record.hrl").

-record(protocol_buffers, {
	  tls_packets = [], %%           :: [#ssl_tls{}],  % Not yet handled decode SSL/TLS packets.
          tls_record_buffer = <<>>, %%    :: binary(),  % Buffer of incomplete records
          tls_handshake_buffer = <<>>, %% :: binary(),  % Buffer of incomplete handshakes
	  tls_cipher_texts = []       %%:: [binary()]
	 }).

-endif. % -ifdef(tls_connection).
