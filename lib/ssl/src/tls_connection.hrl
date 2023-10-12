%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2020. All Rights Reserved.
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

-ifndef(tls_connection).
-define(tls_connection, true).

-include("ssl_connection.hrl").
-include("tls_record.hrl").

-record(protocol_buffers, {
          tls_record_buffer = <<>>, %%    :: binary(),  % Buffer of incomplete records
          tls_handshake_buffer = <<>>, %% :: binary(),  % Buffer of incomplete handshakes
	  tls_cipher_texts = []       %%:: [binary()]
	 }).

-endif. % -ifdef(tls_connection).
