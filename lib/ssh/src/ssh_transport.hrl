%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the SSH-tansport layer 
%% protocol see RFC 4253
%%----------------------------------------------------------------------

-ifndef(ssh_transport).
-define(ssh_transport, true).

-define(DEFAULT_CLIENT_VERSION, {2, 0}).
-define(DEFAULT_SERVER_VERSION, {2, 0}).

-define(MAX_NUM_ALGORITHMS, 200).

-define(DEFAULT_DH_GROUP_MIN,   1024).
-define(DEFAULT_DH_GROUP_NBITS, 2048).
-define(DEFAULT_DH_GROUP_MAX,   8192).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BASIC transport messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SSH_MSG_DISCONNECT,             1).
-define(SSH_MSG_IGNORE,                 2).
-define(SSH_MSG_UNIMPLEMENTED,          3).
-define(SSH_MSG_DEBUG,                  4).
-define(SSH_MSG_SERVICE_REQUEST,        5).
-define(SSH_MSG_SERVICE_ACCEPT,         6).

-define(SSH_MSG_KEXINIT,                20).
-define(SSH_MSG_NEWKEYS,                21).


-record(ssh_msg_disconnect,
	{
	  code,         %% uint32
	  description,  %% string
	  language      %% string
	 }).

-record(ssh_msg_ignore,
	{
	  data          %% string
	 }).

-record(ssh_msg_unimplemented,
	{
	  sequence     %% uint32
	 }).

-record(ssh_msg_debug,
	{
	  always_display,  %% boolean
	  message,         %% string
	  language         %% string
	 }).


-record(ssh_msg_service_request,
	{
	  name     %% string (service name)
	 }).

-record(ssh_msg_service_accept,
	{
	  name     %% string
	 }).

-record(ssh_msg_kexinit,
	{
	  cookie,                                   %% random(16)
	  kex_algorithms,                           %% string
	  server_host_key_algorithms,               %% string    
	  encryption_algorithms_client_to_server,   %% string    
	  encryption_algorithms_server_to_client,   %% string    
	  mac_algorithms_client_to_server,          %% string
	  mac_algorithms_server_to_client,          %% string    
	  compression_algorithms_client_to_server,  %% string
	  compression_algorithms_server_to_client,  %% string
	  languages_client_to_server,               %% string
	  languages_server_to_client,               %% string
	  first_kex_packet_follows=false,           %% boolean
	  %% (reserved for future extension)
	  reserved=0                                %% uint32=0
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group1-sha1 | diffie-hellman-group14-sha1

-define(SSH_MSG_KEXDH_INIT,   30).
-define(SSH_MSG_KEXDH_REPLY,  31).

-record(ssh_msg_kexdh_init,
	{
	  e  %% mpint
	 }).

-record(ssh_msg_kexdh_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,                %% mpint
	  h_sig             %% string, signature of H
	 }).

-record(ssh_msg_newkeys,
	{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH GEX messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group-exchange-sha1 | diffie-hellman-group-exchange-sha256
-define(SSH_MSG_KEX_DH_GEX_REQUEST_OLD, 30).
-define(SSH_MSG_KEX_DH_GEX_REQUEST,     34).
-define(SSH_MSG_KEX_DH_GEX_GROUP,       31).
-define(SSH_MSG_KEX_DH_GEX_INIT,        32).
-define(SSH_MSG_KEX_DH_GEX_REPLY,       33).

-record(ssh_msg_kex_dh_gex_request,
	{
	  min,
	  n,
	  max
	 }).

-record(ssh_msg_kex_dh_gex_request_old,
	{
	  n
	 }).

-record(ssh_msg_kex_dh_gex_group,
	{
	  p,  %% prime
	  g   %% generator
	 }).

-record(ssh_msg_kex_dh_gex_init,
	{
	  e
	 }).

-record(ssh_msg_kex_dh_gex_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,
	  h_sig
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY ECDH messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ecdh-sha2-nistp256 | ecdh-sha2-nistp384 | ecdh-sha2-nistp521

-define(SSH_MSG_KEX_ECDH_INIT,                30).
-define(SSH_MSG_KEX_ECDH_REPLY,               31).

-record(ssh_msg_kex_ecdh_init,
	{
	  q_c    % string (client's ephemeral public key octet string)
	}).

-record(ssh_msg_kex_ecdh_reply,
	{
	  public_host_key,   % string (server's public host key) (k_s)
	  q_s,               % string (server's ephemeral public key octet string)
	  h_sig              % string (the signature on the exchange hash)
	}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% error codes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT,   1).
-define(SSH_DISCONNECT_PROTOCOL_ERROR,   2).
-define(SSH_DISCONNECT_KEY_EXCHANGE_FAILED,   3).
-define(SSH_DISCONNECT_RESERVED,   4).
-define(SSH_DISCONNECT_MAC_ERROR,   5).
-define(SSH_DISCONNECT_COMPRESSION_ERROR,   6).
-define(SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,   7).
-define(SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,   8).
-define(SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE,   9).
-define(SSH_DISCONNECT_CONNECTION_LOST,  10).
-define(SSH_DISCONNECT_BY_APPLICATION,  11).
-define(SSH_DISCONNECT_TOO_MANY_CONNECTIONS,  12).
-define(SSH_DISCONNECT_AUTH_CANCELLED_BY_USER,  13).
-define(SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,  14).
-define(SSH_DISCONNECT_ILLEGAL_USER_NAME,  15).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% groups
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% rfc 2489, ch 6.2
%%% Size 1024
-define(dh_group1,
	 {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF}).

%%% rfc 3526, ch3
%%% Size 2048
-define(dh_group14,
	 {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF}).

-endif. % -ifdef(ssh_transport).
