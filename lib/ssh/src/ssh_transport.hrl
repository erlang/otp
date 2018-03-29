%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-define(SSH_MSG_EXT_INFO,               7).

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

-record(ssh_msg_ext_info,
       {
         nr_extensions, %% uint32

         %% repeat the following 2 fields "nr-extensions" times:
         %%   string   extension-name
         %%   string   extension-value

         data  %% [{extension-name,    %% string
               %%   extension-value},  %% string
               %%         ...
               %% ]
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

%% diffie-hellman-group*-sha*

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

-define(DISCONNECT(Code, DetailedText),
        ssh_connection_handler:disconnect(Code, DetailedText, ?MODULE, ?LINE)).

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

%%% rfc 3526, ch5
%%% Size 4096-bit
-define(dh_group16,
        {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199FFFFFFFFFFFFFFFF}).

%%% rfc 3526, ch7
%%% Size 8192-bit
-define(dh_group18,
        {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C93402849236C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AACC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E6DBE115974A3926F12FEE5E438777CB6A932DF8CD8BEC4D073B931BA3BC832B68D9DD300741FA7BF8AFC47ED2576F6936BA424663AAB639C5AE4F5683423B4742BF1C978238F16CBE39D652DE3FDB8BEFC848AD922222E04A4037C0713EB57A81A23F0C73473FC646CEA306B4BCBC8862F8385DDFA9D4B7FA2C087E879683303ED5BDD3A062B3CF5B3A278A66D2A13F83F44F82DDF310EE074AB6A364597E899A0255DC164F31CC50846851DF9AB48195DED7EA1B1D510BD7EE74D73FAF36BC31ECFA268359046F4EB879F924009438B481C6CD7889A002ED5EE382BC9190DA6FC026E479558E4475677E9AA9E3050E2765694DFC81F56E880B96E7160C980DD98EDD3DFFFFFFFFFFFFFFFFF}).


-endif. % -ifdef(ssh_transport).
