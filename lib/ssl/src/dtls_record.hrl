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
%% Purpose: Record and constant defenitions for the DTLS-record protocol
%% see RFC 6347
%%----------------------------------------------------------------------

-ifndef(dtls_record).
-define(dtls_record, true).

-include("ssl_record.hrl"). %% Common TLS and DTLS records and Constantes

%% Used to handle tls_plain_text, tls_compressed and tls_cipher_text

-record(ssl_tls, {   
	  type,
	  version,
	  record_seq,      % used in plain_text
	  epoch,           % used in plain_text  
	  message_seq, 
	  fragment_offset,
	  fragment_length,
	  fragment
	 }).

-endif. % -ifdef(dtls_record).
