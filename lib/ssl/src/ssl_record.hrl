%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the SSL-record protocol
%% see RFC 2246
%%----------------------------------------------------------------------

-ifndef(ssl_record).
-define(ssl_record, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Connection states - RFC 4346 section 6.1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(connection_states, {
	  current_read,
	  pending_read,
	  current_write,
	  pending_write
	 }).

-record(security_parameters, {
          cipher_suite,
          connection_end,
          bulk_cipher_algorithm,
          cipher_type,
          iv_size,
          key_size,				% unit 8
          key_material_length,			% unit 8 
          expanded_key_material_length,		% unit 8 
          mac_algorithm,			% unit 8  
          hash_size,				% unit 8
          compression_algorithm,		% unit 8 
          master_secret,			% opaque 48
          client_random,			% opaque 32
          server_random,			% opaque 32
          exportable				% boolean
       }). 

-record(connection_state, {
	  security_parameters,
	  compression_state,
	  cipher_state,
	  mac_secret,
	  sequence_number,
	  %% RFC 5746
	  secure_renegotiation,
	  client_verify_data,
	  server_verify_data
	 }).

-define(MAX_SEQENCE_NUMBER, 18446744073709552000). %% math:pow(2, 64) - 1 = 1.8446744073709552e19
%% Sequence numbers can not wrap so when max is about to be reached we should renegotiate.
%% We will renegotiate a little before so that there will be sequence numbers left
%% for the rehandshake and a little data. 
-define(MARGIN, 100).
-define(DEFAULT_RENEGOTIATE_AT, ?MAX_SEQENCE_NUMBER - ?MARGIN).

%% ConnectionEnd
-define(SERVER, 0).
-define(CLIENT, 1).

%% BulkCipherAlgorithm
%-define(NULL, 0). %% Already defined by ssl_internal.hrl
-define(RC4, 1).
-define(RC2, 2).
-define(DES, 3).
-define('3DES', 4).
-define(DES40, 5).
-define(IDEA, 6).
-define(AES, 7). 

%% CipherType
-define(STREAM, 0).
-define(BLOCK, 1).

%% IsExportable
%-define(TRUE, 0).  %% Already defined by ssl_internal.hrl
%-define(FALSE, 1). %% Already defined by ssl_internal.hrl

%% MACAlgorithm
%-define(NULL, 0). %% Already defined by ssl_internal.hrl
-define(MD5, 1).
-define(SHA, 2).

%% CompressionMethod
% -define(NULL, 0). %% Already defined by ssl_internal.hrl


-record(compression_state, {
	  method,
	  state
	 }).

%% See also cipher.hrl for #cipher_state{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Record layer - RFC 2246 section 6.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%enum {
%%      change_cipher_spec(20), alert(21), handshake(22),
%%      application_data(23), (255)
%%       } ContentType;

-define(CHANGE_CIPHER_SPEC, 20). 
-define(ALERT, 21).
-define(HANDSHAKE, 22).
-define(APPLICATION_DATA, 23).
-define(MAX_PLAIN_TEXT_LENGTH, 16384).
-define(MAX_COMPRESSED_LENGTH, (?MAX_PLAIN_TEXT_LENGTH+1024)).
-define(MAX_CIPHER_TEXT_LENGTH, (?MAX_PLAIN_TEXT_LENGTH+2048)).

%% -record(protocol_version, {
%% 	  major,  % unit 8
%% 	  minor   % unit 8
%% 	 }).

-define(LOWEST_MAJOR_SUPPORTED_VERSION, 3).
	
-record(ssl_tls, {   %% From inet driver
	  port,
	  type,
	  version, 
	  fragment
	 }).

%% -record(tls_plain_text, {
%% 	  type, 
%% 	  version,   % #protocol_version{} 
%% 	  length,    % unit 16  
%% 	  fragment   % opaque  
%% 	 }).

%% -record(tls_compressed, {
%% 	  type,
%% 	  version,
%% 	  length,    % unit 16  
%% 	  fragment   % opaque  
%% 	 }).

%% -record(tls_cipher_text, {
%%           type,
%%           version,
%%           length,
%%           cipher,
%%           fragment
%%          }).

-record(generic_stream_cipher, {
          content,  % opaque content[TLSCompressed.length];
          mac       % opaque MAC[CipherSpec.hash_size]; 
         }).

-record(generic_block_cipher, {
          iv,            % opaque IV[CipherSpec.block_length];
          content, % opaque content[TLSCompressed.length];
          mac,     % opaque MAC[CipherSpec.hash_size];
          padding, % unit 8 padding[GenericBlockCipher.padding_length];
          padding_length % uint8 padding_length;
         }). 

-endif. % -ifdef(ssl_record).
