%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2015. All Rights Reserved.
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

-module(ssl_eqc_handshake).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include_lib("ssl/src/tls_handshake.hrl").
-include_lib("ssl/src/ssl_handshake.hrl").
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_internal.hrl").

-define('TLS_v1.3', {3,4}).
-define('TLS_v1.2', {3,3}).
-define('TLS_v1.1', {3,2}).
-define('TLS_v1',   {3,1}).
-define('SSL_v3',   {3,0}).

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_tls_hs_encode_decode() ->
    ?FORALL({Handshake, TLSVersion}, ?LET(Version, tls_version(), {tls_msg(Version), Version}),
            try 
                [Type, _Length, Data] = tls_handshake:encode_handshake(Handshake, TLSVersion),
                case tls_handshake:decode_handshake(TLSVersion, Type, Data) of
                    Handshake ->
                        true;
                    _ ->
                        false
                end
            catch
                throw:#alert{} ->
                    true
            end
	   ).

tls_version() ->
    oneof([?'TLS_v1.2', ?'TLS_v1.1', ?'TLS_v1', ?'SSL_v3']). 

tls_msg(?'TLS_v1.3'= Version) ->
    oneof([client_hello(Version),
           %%server_hello(Version)
           %%new_session_ticket()
           #end_of_early_data{},
           %%encrypted_extensions()
           %%certificate_1_3(),
           %%certificate_request()
           %%certificate_verify()
           %%finished()
           key_update()
           %%message_hash()
          ]);
tls_msg(Version) ->
    oneof([#hello_request{},
           client_hello(Version),
           %%server_hello(Version)
           %%certificate(),
           %%server_key_exchange()
           %%certificate_request()
           #server_hello_done{}
           %%certificate_verify()
           %%client_key_exchange()
           %%finished()
          ]).

client_hello(?'TLS_v1.3' = Version) ->
    #client_hello{session_id = session_id(),
		  client_version = ?'TLS_v1.2',
		  cipher_suites = ssl_cipher:suites(Version), 
		  compression_methods = compressions(Version),
		  random = client_random(Version),
		  extensions = client_extensions(Version)    
                 };
client_hello(Version) ->
    #client_hello{session_id = session_id(),
		  client_version = Version,
		  cipher_suites = ssl_cipher:suites(Version), 
		  compression_methods = compressions(Version),
		  random = client_random(Version),
		  extensions = client_extensions(Version)    
                 }.
session_id() ->
    crypto:strong_rand_bytes(?NUM_OF_SESSION_ID_BYTES).
 
compressions(_) -> 
    ssl_record:compressions().
client_random(_) ->
    crypto:strong_rand_bytes(32).
       
client_extensions(?'TLS_v1.3' = Version) ->
    #hello_extensions{
       client_hello_versions =
           #client_hello_versions{
              versions = supported_versions(Version)
             },
       signature_algs_cert =
           #signature_scheme_list{
              signature_scheme_list = signature_scheme_list()
             }
      };
client_extensions(Version) ->
    #hello_extensions{
       client_hello_versions =
           #client_hello_versions{
              versions = supported_versions(Version)
             }
      }.

signature_scheme_list() ->
    oneof([[rsa_pkcs1_sha256],
           [rsa_pkcs1_sha256, ecdsa_sha1],
           [rsa_pkcs1_sha256,
            rsa_pkcs1_sha384,
            rsa_pkcs1_sha512,
            ecdsa_secp256r1_sha256,
            ecdsa_secp384r1_sha384,
            ecdsa_secp521r1_sha512,
            rsa_pss_rsae_sha256,
            rsa_pss_rsae_sha384,
            rsa_pss_rsae_sha512,
            rsa_pss_pss_sha256,
            rsa_pss_pss_sha384,
            rsa_pss_pss_sha512,
            rsa_pkcs1_sha1,
            ecdsa_sha1]
          ]).

supported_versions(?'TLS_v1.3') ->
    oneof([[{3,4}],
           [{3,3},{3,4}],
           [{3,4},{3,3},{3,2},{3,1},{3,0}]
          ]);
supported_versions(_) ->
    oneof([[{3,3}],
           [{3,3},{3,2}],
           [{3,3},{3,2},{3,1},{3,0}]
          ]).

key_update() ->
    #key_update{request_update = request_update()}.

request_update() ->
     oneof([?UPDATE_NOT_REQUESTED, ?UPDATE_REQUESTED]).
