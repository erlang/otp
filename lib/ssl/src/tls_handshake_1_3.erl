%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Help functions for handling the TLS 1.3 (specific parts of)
%%% TLS handshake protocol
%%----------------------------------------------------------------------

-module(tls_handshake_1_3).
-moduledoc false.

-include("tls_handshake_1_3.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").
-include("ssl_connection.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("tls_record_1_3.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Encode
-export([encode_handshake/1, decode_handshake/2]).

%% Create handshake messages
-export([server_hello/5,
         maybe_add_cookie_extension/2,
         maybe_add_binders/3,
         maybe_add_binders/4,
         maybe_add_early_data_indication/3,
         supported_groups_from_extensions/1,
         maybe_hello_retry_request/2,
         certificate/5,
         certificate_verify/4,
         certificate_request/5,
         encrypted_extensions/1,
         key_update/1,
         finished/1,
         create_change_cipher_spec/1
        ]).

%% Handle handshake messages
-export([process_certificate_request/2,
         process_certificate/2,
         calculate_handshake_secrets/5,
         calculate_exporter_master_secret/1,
         verify_certificate_verify/2,
         validate_finished/2,
         maybe_calculate_resumption_master_secret/1,
         replace_ch1_with_message_hash/1,
         select_common_groups/2,
         verify_signature_algorithm/2,
         forget_master_secret/1,
         set_client_random/2,
         handle_pre_shared_key/3,
         update_start_state/2,
         get_signature_scheme_list/1,
         get_certificate_authorities/1,
         get_certificate_params/1,
         select_sign_algo/5
        ]).

-export([early_data_size/1,
         get_pre_shared_key/2,
         get_pre_shared_key/4,
         get_pre_shared_key_early_data/2,
         get_supported_groups/1,
         calculate_traffic_secrets/1,
         calculate_client_early_traffic_secret/5,
         calculate_client_early_traffic_secret/2,
         encode_early_data/2,
         get_ticket_data/3,
         ciphers_for_early_data/1,
         choose_ticket/2]).

-export([get_max_early_data/1,
         is_valid_binder/4,
         check_cert_sign_algo/4,
         path_validation/10]).

%% crypto:hash(sha256, "HelloRetryRequest").
-define(HELLO_RETRY_REQUEST_RANDOM, <<207,33,173,116,229,154,97,17,
                                      190,29,140,2,30,101,184,145,
                                      194,162,17,22,122,187,140,94,
                                      7,158,9,226,200,168,51,156>>).

%%====================================================================
%% Create handshake messages
%%====================================================================

server_hello(MsgType, SessionId, KeyShare, PSK, ConnectionStates) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    Extensions = server_hello_extensions(MsgType, KeyShare, PSK),
    #server_hello{server_version = ?LEGACY_VERSION, %% legacy_version
		  cipher_suite = SecParams#security_parameters.cipher_suite,
		  random = server_hello_random(MsgType, SecParams),
		  session_id = SessionId,
		  extensions = Extensions
		 }.


%% The server's extensions MUST contain "supported_versions".
%% Additionally, it SHOULD contain the minimal set of extensions
%% necessary for the client to generate a correct ClientHello pair.  As
%% with the ServerHello, a HelloRetryRequest MUST NOT contain any
%% extensions that were not first offered by the client in its
%% ClientHello, with the exception of optionally the "cookie" (see
%% Section 4.2.2) extension.
server_hello_extensions(hello_retry_request = MsgType, KeyShare, _) ->
    Extensions = server_hello_extensions_versions(),
    ssl_handshake:add_server_share(MsgType, Extensions, KeyShare);
server_hello_extensions(MsgType, KeyShare, undefined) ->
    Extensions = server_hello_extensions_versions(),
    ssl_handshake:add_server_share(MsgType, Extensions, KeyShare);
server_hello_extensions(MsgType, KeyShare, {SelectedIdentity, _}) ->
    Extensions = server_hello_extensions_versions(),
    PreSharedKey = #pre_shared_key_server_hello{selected_identity = SelectedIdentity},
    ssl_handshake:add_server_share(MsgType, Extensions#{pre_shared_key => PreSharedKey}, KeyShare).

server_hello_extensions_versions() ->
    SupportedVersions = #server_hello_selected_version{selected_version = ?TLS_1_3},
    #{server_hello_selected_version => SupportedVersions}.



server_hello_random(server_hello, #security_parameters{server_random = Random}) ->
    Random;
%% For reasons of backward compatibility with middleboxes (see
%% Appendix D.4), the HelloRetryRequest message uses the same structure
%% as the ServerHello, but with Random set to the special value of the
%% SHA-256 of "HelloRetryRequest":
%%
%%   CF 21 AD 74 E5 9A 61 11 BE 1D 8C 02 1E 65 B8 91
%%   C2 A2 11 16 7A BB 8C 5E 07 9E 09 E2 C8 A8 33 9C
server_hello_random(hello_retry_request, _) ->
    ?HELLO_RETRY_REQUEST_RANDOM.

maybe_add_cookie_extension(#state{ssl_options = #{cookie := false}} = State,
                           ServerHello) ->
    {State, ServerHello};
maybe_add_cookie_extension(#state{connection_states = ConnectionStates,
                                  ssl_options = #{cookie := true},
                                  handshake_env =
                                      #handshake_env{
                                         tls_handshake_history =
                                             {[CH1|_], _}} = HsEnv0} = State,
                           #server_hello{extensions = Extensions0} = ServerHello) ->
    HKDFAlgo = get_hkdf_algorithm(ConnectionStates),
    MessageHash0 = message_hash(CH1, HKDFAlgo),
    MessageHash = iolist_to_binary(MessageHash0),

    %% Encrypt MessageHash
    IV = crypto:strong_rand_bytes(16),
    Shard = crypto:strong_rand_bytes(32),
    Cookie = ssl_cipher:encrypt_data(<<"cookie">>, MessageHash, Shard, IV),

    HsEnv = HsEnv0#handshake_env{cookie_iv_shard = {IV, Shard}},
    Extensions = Extensions0#{cookie => #cookie{cookie = Cookie}},
    {State#state{handshake_env = HsEnv},
     ServerHello#server_hello{extensions = Extensions}};
maybe_add_cookie_extension(undefined, ClientHello) ->
    ClientHello;
maybe_add_cookie_extension(Cookie,
                           #client_hello{extensions = Extensions0} = ClientHello) ->
    Extensions = Extensions0#{cookie => Cookie},
    ClientHello#client_hello{extensions = Extensions}.

encrypted_extensions(#state{handshake_env = HandshakeEnv}) ->
    E0 = #{},
    E1 = case HandshakeEnv#handshake_env.alpn of
             undefined ->
                 E0;
             ALPNProtocol ->
                 ssl_handshake:add_alpn(#{}, ALPNProtocol)
         end,
    E2 = case HandshakeEnv#handshake_env.max_frag_enum of
             undefined ->
                 E1;
             MaxFragEnum ->
                 E1#{max_frag_enum => MaxFragEnum}
         end,
    E3 = case HandshakeEnv#handshake_env.sni_guided_cert_selection of
             false ->
                 E2;
             true ->
                 E2#{sni => #sni{hostname = ""}}
        end,
    E = case HandshakeEnv#handshake_env.early_data_accepted of
            false ->
                E3;
            true ->
                E3#{early_data => #early_data_indication{}}
        end,
    #encrypted_extensions{
       extensions = E
      }.

certificate_request(SignAlgs0, SignAlgsCert0, CertDbHandle, CertDbRef, CertAuthBool) ->
    %% Input arguments contain TLS 1.2 algorithms due to backward compatibility
    %% reasons. These {Hash, Algo} tuples must be filtered before creating the
    %% the extensions.
    SignAlgs = filter_tls13_algs(SignAlgs0),
    SignAlgsCert = filter_tls13_algs(SignAlgsCert0),
    Extensions0 = add_signature_algorithms(#{}, SignAlgs),
    Extensions1 = add_signature_algorithms_cert(Extensions0, SignAlgsCert),
    Extensions = if CertAuthBool =:= true ->
                        Auths = ssl_handshake:certificate_authorities(CertDbHandle, CertDbRef),
                        Extensions1#{certificate_authorities => #certificate_authorities{authorities = Auths}};
                    true ->
                        Extensions1
                 end,
    #certificate_request_1_3{
      certificate_request_context = <<>>,
      extensions = Extensions}.


add_signature_algorithms(Extensions, SignAlgs) ->
    Extensions#{signature_algorithms =>
                    #signature_algorithms{signature_scheme_list = SignAlgs}}.


add_signature_algorithms_cert(Extensions, undefined) ->
    Extensions;
add_signature_algorithms_cert(Extensions, SignAlgsCert) ->
    Extensions#{signature_algorithms_cert =>
                    #signature_algorithms_cert{signature_scheme_list = SignAlgsCert}}.


filter_tls13_algs(undefined) -> undefined;
filter_tls13_algs(Algo) ->
    lists:foldl(fun(Atom, Acc) when is_atom(Atom) ->
                        [Atom | Acc];
                   ({sha512, rsa}, Acc) ->
                        [rsa_pkcs1_sha512 | Acc];
                   ({sha384, rsa}, Acc) ->
                        [rsa_pkcs1_sha384 | Acc];
                   ({sha256, rsa}, Acc) ->
                        [rsa_pkcs1_sha256 | Acc];
                   ({sha, rsa}, Acc) ->
                        [rsa_pkcs1_sha1 | Acc];
                   ({sha, ecdsa}, Acc) ->
                        [ecdsa_sha1| Acc];
                   (_, Acc) ->
                        Acc
                end, [], Algo).
%% enum {
%%     X509(0),
%%     RawPublicKey(2),
%%     (255)
%% } CertificateType;
%%
%% struct {
%%     select (certificate_type) {
%%         case RawPublicKey:
%%           /* From RFC 7250 ASN.1_subjectPublicKeyInfo */
%%           opaque ASN1_subjectPublicKeyInfo<1..2^24-1>;
%%
%%         case X509:
%%           opaque cert_data<1..2^24-1>;
%%     };
%%     Extension extensions<0..2^16-1>;
%% } CertificateEntry;
%%
%% struct {
%%     opaque certificate_request_context<0..2^8-1>;
%%     CertificateEntry certificate_list<0..2^24-1>;
%% } Certificate;
certificate(undefined, _, _, _, client) ->
    {ok, #certificate_1_3{
            certificate_request_context = <<>>,
            certificate_list = []}};
certificate([OwnCert], CertDbHandle, CertDbRef, _CRContext, Role) ->
    case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	{ok, _, Chain} ->
            CertList = chain_to_cert_list(Chain),
            %% If this message is in response to a CertificateRequest, the value of
            %% certificate_request_context in that message. Otherwise (in the case
            %%of server authentication), this field SHALL be zero length.
	    {ok, #certificate_1_3{
                    certificate_request_context = <<>>,
                    certificate_list = CertList}};
	{error, Error} when Role =:= server ->
            {error, ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {no_suitable_certificates, Error})};
	{error, _Error} when Role =:= client ->
            %% The client MUST send a Certificate message if and only if the server
            %% has requested client authentication via a CertificateRequest message
            %% (Section 4.3.2).  If the server requests client authentication but no
            %% suitable certificate is available, the client MUST send a Certificate
            %% message containing no certificates (i.e., with the "certificate_list"
            %% field having length 0).
            {ok, #certificate_1_3{
                    certificate_request_context = <<>>,
                    certificate_list = []}}
    end;
certificate([_,_| _] = Chain, _,_,_,_) ->
    CertList = chain_to_cert_list(Chain),
    {ok, #certificate_1_3{
            certificate_request_context = <<>>,
            certificate_list = CertList}}.

certificate_verify(PrivateKey, SignatureScheme,
                   #state{connection_states = ConnectionStates,
                          handshake_env =
                              #handshake_env{
                                 tls_handshake_history = {Messages, _}}}, Role) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, write),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    {HashAlgo, SignAlgo, _} =
        ssl_cipher:scheme_to_components(SignatureScheme),

    Context = lists:reverse(Messages),

    %% Transcript-Hash uses the HKDF hash function defined by the cipher suite.
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),
    ContextString = context_string(Role),

    %% Digital signatures use the hash function defined by the selected signature
    %% scheme.
    case sign(THash, ContextString, HashAlgo, PrivateKey, SignAlgo) of
        {ok, Signature} ->
            {ok, #certificate_verify_1_3{
                    algorithm = SignatureScheme,
                    signature = Signature
                   }};
        {error, #alert{} = Alert} ->
            {error, Alert}
    end.

%% For reasons of backward compatibility with middleboxes (see
%% Appendix D.4), the HelloRetryRequest message uses the same structure
%% as the ServerHello, but with Random set to the special value of the
%% SHA-256 of "HelloRetryRequest":
%%
%%   CF 21 AD 74 E5 9A 61 11 BE 1D 8C 02 1E 65 B8 91
%%   C2 A2 11 16 7A BB 8C 5E 07 9E 09 E2 C8 A8 33 9C
%%
%% Upon receiving a message with type server_hello, implementations MUST
%% first examine the Random value and, if it matches this value, process
%% it as described in Section 4.1.4).
maybe_hello_retry_request(#server_hello{random = ?HELLO_RETRY_REQUEST_RANDOM} = ServerHello, 
                          #state{protocol_specific = PS} = State0) ->
    {error, {State0#state{protocol_specific = PS#{hello_retry => true}}, start, ServerHello}};
maybe_hello_retry_request(_, _) ->
    ok.

finished(#state{connection_states = ConnectionStates,
                handshake_env =
                    #handshake_env{
                       tls_handshake_history = {Messages, _}}}) ->
    #{security_parameters := SecParamsR,
     cipher_state := #cipher_state{finished_key = FinishedKey}} =
        ssl_record:current_connection_state(ConnectionStates, write),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    VerifyData = tls_v1:finished_verify_data(FinishedKey, HKDFAlgo, Messages),

    #finished{
       verify_data = VerifyData
      }.


key_update(Type) ->
    #key_update{request_update = Type}.

create_change_cipher_spec(#state{ssl_options = #{log_level := LogLevel}}) ->
    %% Dummy connection_states with NULL cipher
    ConnectionStates =
        #{current_write =>
              #{cipher_state => undefined,
                sequence_number => 1,
                security_parameters =>
                    #security_parameters{
                       bulk_cipher_algorithm = 0,
                       mac_algorithm = ?NULL
                      },
                mac_secret => undefined}},
    {BinChangeCipher, _} =
        tls_record:encode_change_cipher_spec(?LEGACY_VERSION, ConnectionStates),
    ssl_logger:debug(LogLevel, outbound, 'record', BinChangeCipher),
    [BinChangeCipher].

%%====================================================================
%% Handle handshake messages 
%%====================================================================

process_certificate_request(#certificate_request_1_3{
                               extensions = Extensions},
                            #state{ssl_options = #{signature_algs := ClientSignAlgs},
                                   connection_env = #connection_env{cert_key_alts = CertKeyAlts,
                                                                    negotiated_version = Version},
                                   static_env = #static_env{cert_db = CertDbHandle, cert_db_ref = CertDbRef},
                                   session = Session0} =
                                State) ->
    ServerSignAlgs = get_signature_scheme_list(
                       maps:get(signature_algs, Extensions, undefined)),
    ServerSignAlgsCert = get_signature_scheme_list(
                           maps:get(signature_algs_cert, Extensions, undefined)),
    CertAuths = get_certificate_authorities(maps:get(certificate_authorities, Extensions, undefined)),

    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, Version),
    Session = select_client_cert_key_pair(Session0, CertKeyPairs,
                                          ServerSignAlgs, ServerSignAlgsCert, ClientSignAlgs,
                                          CertDbHandle, CertDbRef, CertAuths, undefined),
    {ok, {State#state{client_certificate_status = requested, session = Session}, wait_cert}}.

process_certificate(#certificate_1_3{
                       certificate_request_context = <<>>,
                       certificate_list = []},
                    #state{ssl_options =
                               #{fail_if_no_peer_cert := false}} = State) ->
    {ok, {State, wait_finished}};
process_certificate(#certificate_1_3{
                       certificate_request_context = <<>>,
                       certificate_list = []},
                    #state{ssl_options =
                               #{fail_if_no_peer_cert := true}} = State0) ->
    %% At this point the client believes that the connection is up and starts using
    %% its traffic secrets. In order to be able send an proper Alert to the client
    %% the server should also change its connection state and use the traffic
    %% secrets.
    State1 = calculate_traffic_secrets(State0),
    State = ssl_record:step_encryption_state(State1),
    {error, {?ALERT_REC(?FATAL, ?CERTIFICATE_REQUIRED, certificate_required), State}};
process_certificate(#certificate_1_3{certificate_list = CertEntries},
                    #state{ssl_options = SslOptions,
                           static_env =
                               #static_env{
                                  role = Role,
                                  host = Host,
                                  cert_db = CertDbHandle,
                                  cert_db_ref = CertDbRef,
                                  crl_db = CRLDbHandle},
                           handshake_env =
                               #handshake_env{
                                  stapling_state = StaplingState}} = State0) ->
    case validate_certificate_chain(
           CertEntries, CertDbHandle, CertDbRef, SslOptions, CRLDbHandle, Role,
           Host, StaplingState) of
        #alert{} = Alert ->
            State = update_encryption_state(Role, State0),
            {error, {Alert, State}};
        {PeerCert, PublicKeyInfo} ->
            State = store_peer_cert(State0, PeerCert, PublicKeyInfo),
            {ok, {State, wait_cv}}
    end.

verify_certificate_verify(#state{static_env = #static_env{role = Role},
                                 connection_states = ConnectionStates,
                                 handshake_env =
                                     #handshake_env{
                                        public_key_info = PublicKeyInfo,
                                        tls_handshake_history = HHistory}} = State0,
                          #certificate_verify_1_3{
                             algorithm = SignatureScheme,
                             signature = Signature}) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, write),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    {HashAlgo, SignAlg, _} =
        ssl_cipher:scheme_to_components(SignatureScheme),

    Messages = get_handshake_context_cv(HHistory),

    Context = lists:reverse(Messages),

    %% Transcript-Hash uses the HKDF hash function defined by the cipher suite.
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),

    ContextString = peer_context_string(Role),

    %% Digital signatures use the hash function defined by the selected signature
    %% scheme.
    case verify(THash, ContextString, HashAlgo, SignAlg, Signature, PublicKeyInfo) of
        {ok, true} ->
            {ok, {State0, wait_finished}};
        {ok, false} ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,
                                "Failed to verify CertificateVerify"), State}};
        {error, #alert{} = Alert} ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {Alert, State}}
    end.

%% Recipients of Finished messages MUST verify that the contents are
%% correct and if incorrect MUST terminate the connection with a
%% "decrypt_error" alert.
validate_finished(#state{connection_states = ConnectionStates,
                         handshake_env =
                             #handshake_env{
                                tls_handshake_history = {Messages0, _}}}, VerifyData) ->
    #{security_parameters := SecParamsR,
      cipher_state := #cipher_state{finished_key = FinishedKey}} =
        ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    %% Drop the peer's finished message, it is not part of the handshake context
    %% when the client/server calculates its finished message.
    [_|Messages] = Messages0,

    ControlData = tls_v1:finished_verify_data(FinishedKey, HKDFAlgo, Messages),
    compare_verify_data(ControlData, VerifyData).


compare_verify_data(Data, Data) ->
    ok;
compare_verify_data(_, _) ->
    {error, ?ALERT_REC(?FATAL, ?DECRYPT_ERROR, decrypt_error)}.

%%====================================================================
%% Encode handshake
%%====================================================================

encode_handshake(#certificate_request_1_3{
                    certificate_request_context = Context,
                    extensions = Exts})->
    EncContext = encode_cert_req_context(Context),
    BinExts = encode_extensions(Exts),
    {?CERTIFICATE_REQUEST, <<EncContext/binary, BinExts/binary>>};
encode_handshake(#certificate_1_3{
                    certificate_request_context = Context,
                    certificate_list = Entries}) ->
    EncContext = encode_cert_req_context(Context),
    EncEntries = encode_cert_entries(Entries),
    {?CERTIFICATE, <<EncContext/binary, EncEntries/binary>>};
encode_handshake(#certificate_verify_1_3{
                    algorithm = Algorithm,
                    signature = Signature}) ->
    EncAlgo = encode_algorithm(Algorithm),
    EncSign = encode_signature(Signature),
    {?CERTIFICATE_VERIFY, <<EncAlgo/binary, EncSign/binary>>};
encode_handshake(#encrypted_extensions{extensions = Exts})->
    {?ENCRYPTED_EXTENSIONS, encode_extensions(Exts)};
encode_handshake(#new_session_ticket{
                    ticket_lifetime = LifeTime,
                    ticket_age_add = Age,
                    ticket_nonce = Nonce,
                    ticket = Ticket,
                    extensions = Exts}) ->
    TicketSize = byte_size(Ticket),
    NonceSize = byte_size(Nonce),
    BinExts = encode_extensions(Exts),
    {?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                            ?BYTE(NonceSize), Nonce/binary,
                            ?UINT16(TicketSize), Ticket/binary,
                            BinExts/binary>>};
encode_handshake(#end_of_early_data{}) ->
    {?END_OF_EARLY_DATA, <<>>};
encode_handshake(#key_update{request_update = Update}) ->
    EncUpdate = encode_key_update(Update),
    {?KEY_UPDATE, <<EncUpdate/binary>>};
encode_handshake(HandshakeMsg) ->
    ssl_handshake:encode_handshake(HandshakeMsg, ?TLS_1_3).

encode_early_data(Cipher,
                  #state{
                     flight_buffer = Flight0,
                     protocol_specific = #{sender := _Sender},
                     ssl_options = #{versions := [Version|_],
                                     early_data := EarlyData} = _SslOpts0
                    } = State0) ->
    #state{connection_states =
               #{current_write :=
                     #{security_parameters := SecurityParameters0} = Write0} = ConnectionStates0} = State0,
    BulkCipherAlgo = ssl_cipher:bulk_cipher_algorithm(Cipher),
    SecurityParameters = SecurityParameters0#security_parameters{
                           cipher_type = ?AEAD,
                           bulk_cipher_algorithm = BulkCipherAlgo},
    Write = Write0#{security_parameters => SecurityParameters},
    ConnectionStates1 = ConnectionStates0#{current_write => Write},
    {BinEarlyData, ConnectionStates} = tls_record:encode_data([EarlyData], Version, ConnectionStates1),
    State0#state{connection_states = ConnectionStates,
		 flight_buffer = Flight0 ++ [BinEarlyData]}.


%%====================================================================
%% Decode handshake
%%====================================================================


decode_handshake(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                                  ?BYTE(SID_length), Session_ID:SID_length/binary,
                                  Cipher_suite:2/binary, ?BYTE(_CompMethod),
                                  ?UINT16(ExtLen), Extensions:ExtLen/binary>>)
  when Random =:= ?HELLO_RETRY_REQUEST_RANDOM ->
    HelloExtensions = ssl_handshake:decode_hello_extensions(Extensions, ?TLS_1_3, {Major, Minor},
                                                            hello_retry_request),
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       extensions = HelloExtensions};
decode_handshake(?CERTIFICATE_REQUEST, <<?BYTE(0), ?UINT16(Size), EncExts:Size/binary>>) ->
    Exts = decode_extensions(EncExts, certificate_request),
    #certificate_request_1_3{
       certificate_request_context = <<>>,
       extensions = Exts};
decode_handshake(?CERTIFICATE_REQUEST, <<?BYTE(CSize), Context:CSize/binary,
                                         ?UINT16(Size), EncExts:Size/binary>>) ->
    Exts = decode_extensions(EncExts, certificate_request),
    #certificate_request_1_3{
       certificate_request_context = Context,
       extensions = Exts};
decode_handshake(?CERTIFICATE, <<?BYTE(0), ?UINT24(Size), Certs:Size/binary>>) ->
    CertList = decode_cert_entries(Certs),
    #certificate_1_3{
       certificate_request_context = <<>>,
       certificate_list = CertList
      };
decode_handshake(?CERTIFICATE, <<?BYTE(CSize), Context:CSize/binary,
                                 ?UINT24(Size), Certs:Size/binary>>) ->
    CertList = decode_cert_entries(Certs),
    #certificate_1_3{
       certificate_request_context = Context,
       certificate_list = CertList
      };
decode_handshake(?CERTIFICATE_VERIFY, <<?UINT16(EncAlgo), ?UINT16(Size), Signature:Size/binary>>) ->
    Algorithm = ssl_cipher:signature_scheme(EncAlgo),
    #certificate_verify_1_3{
       algorithm = Algorithm,
       signature = Signature};
decode_handshake(?ENCRYPTED_EXTENSIONS, <<?UINT16(Size), EncExts:Size/binary>>) ->
    #encrypted_extensions{
       extensions = decode_extensions(EncExts, encrypted_extensions)
      };
decode_handshake(?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                                        ?BYTE(NonceSize), Nonce:NonceSize/binary,
                                        ?UINT16(TicketSize), Ticket:TicketSize/binary,
                                        ?UINT16(BinExtSize), BinExts:BinExtSize/binary>>) ->
    Exts = decode_extensions(BinExts, encrypted_extensions),
    #new_session_ticket{ticket_lifetime = LifeTime,
                        ticket_age_add = Age,
                        ticket_nonce = Nonce,
                        ticket = Ticket,
                        extensions = Exts};
decode_handshake(?END_OF_EARLY_DATA, _) ->
    #end_of_early_data{};
decode_handshake(?KEY_UPDATE, <<?BYTE(Update)>>) ->
    #key_update{request_update = decode_key_update(Update)};
decode_handshake(Tag, HandshakeMsg) ->
    ssl_handshake:decode_handshake(?TLS_1_3, Tag, HandshakeMsg).

is_valid_binder(Binder, HHistory, PSK, Hash) ->
    case HHistory of
        [ClientHello2, HRR, MessageHash|_] ->
            Truncated = truncate_client_hello(ClientHello2),
            FinishedKey = calculate_finished_key(PSK, Hash),
            Binder == calculate_binder(FinishedKey, Hash, [MessageHash, HRR, Truncated]);
        [ClientHello1|_] ->
            Truncated = truncate_client_hello(ClientHello1),
            FinishedKey = calculate_finished_key(PSK, Hash),
            Binder == calculate_binder(FinishedKey, Hash, Truncated)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_cert_req_context(<<>>) ->
    <<?BYTE(0)>>;
encode_cert_req_context(Bin) ->
    Size = byte_size(Bin),
    <<?BYTE(Size), Bin/binary>>.

encode_cert_entries(Entries) ->
    CertEntryList = encode_cert_entries(Entries, []),
    Size = byte_size(CertEntryList),
    <<?UINT24(Size), CertEntryList/binary>>.
 
encode_cert_entries([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
encode_cert_entries([#certificate_entry{data = Data,
                                        extensions = Exts} | Rest], Acc) ->
    DSize = byte_size(Data),
    BinExts = encode_extensions(Exts),
    encode_cert_entries(Rest, 
                        [<<?UINT24(DSize), Data/binary, BinExts/binary>> | Acc]).

encode_algorithm(Algo) ->
    Scheme = ssl_cipher:signature_scheme(Algo),
    <<?UINT16(Scheme)>>.

encode_signature(Signature) ->
    Size = byte_size(Signature),
    <<?UINT16(Size), Signature/binary>>.

encode_key_update(update_not_requested) ->
    <<?BYTE(0)>>;
encode_key_update(update_requested) ->
    <<?BYTE(1)>>.

%% enum {
%%     update_not_requested(0), update_requested(1), (255)
%% } KeyUpdateRequest;
%%
%% request_update:  Indicates whether the recipient of the KeyUpdate
%%    should respond with its own KeyUpdate.  If an implementation
%%    receives any other value, it MUST terminate the connection with an
%%    "illegal_parameter" alert.
decode_key_update(0) ->
    update_not_requested;
decode_key_update(1) ->
    update_requested;
decode_key_update(N) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, {request_update,N})).

decode_cert_entries(Entries) ->
    [ #certificate_entry{data = Data, extensions = decode_extensions(BinExts, certificate_request)}
      || <<?UINT24(DSize), Data:DSize/binary, ?UINT16(Esize), BinExts:Esize/binary>> <= Entries ].

encode_extensions(Exts)->
    ssl_handshake:encode_extensions(extensions_list(Exts)).

decode_extensions(Exts, MessageType) ->
    ssl_handshake:decode_extensions(Exts, ?TLS_1_3, MessageType).

extensions_list(Extensions) ->
    [Ext || {_, Ext} <- maps:to_list(Extensions)].

%% TODO: add extensions!
chain_to_cert_list(L) ->
    chain_to_cert_list(L, []).

chain_to_cert_list([], Acc) ->
    lists:reverse(Acc);
chain_to_cert_list([H|T], Acc) ->
    chain_to_cert_list(T, [certificate_entry(H)|Acc]).

certificate_entry(DER) ->
    #certificate_entry{
       data = DER,
       extensions = #{} %% Extensions not supported.
      }.

%% The digital signature is then computed over the concatenation of:
%%   -  A string that consists of octet 32 (0x20) repeated 64 times
%%   -  The context string
%%   -  A single 0 byte which serves as the separator
%%   -  The content to be signed
%%
%% For example, if the transcript hash was 32 bytes of 01 (this length
%% would make sense for SHA-256), the content covered by the digital
%% signature for a server CertificateVerify would be:
%%
%%    2020202020202020202020202020202020202020202020202020202020202020
%%    2020202020202020202020202020202020202020202020202020202020202020
%%    544c5320312e332c207365727665722043657274696669636174655665726966
%%    79
%%    00
%%    0101010101010101010101010101010101010101010101010101010101010101
sign(THash, Context, HashAlgo, PrivateKey, SignAlgo) ->
    Content = build_content(Context, THash),
    try
        {ok, ssl_handshake:digitally_signed(?TLS_1_3, Content, HashAlgo, PrivateKey, SignAlgo)}
    catch throw:Alert ->
            {error, Alert}
    end.


verify(THash, Context, HashAlgo, SignAlgo, Signature, PublicKeyInfo) ->
    Content = build_content(Context, THash),
    try ssl_handshake:verify_signature(?TLS_1_3, Content, {HashAlgo, SignAlgo}, Signature, PublicKeyInfo) of
        Result ->
            {ok, Result}
    catch
        error:Reason:ST ->
            ?SSL_LOG(debug, handshake_error, [{reason, Reason}, {stacktrace, ST}]),
            {error, ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, badarg)}
    end.

build_content(Context, THash) ->
    Prefix = binary:copy(<<32>>, 64),
    <<Prefix/binary,Context/binary,?BYTE(0),THash/binary>>.


%% Sets correct encryption state when sending Alerts in shared states that use different secrets.
%% - If client: use handshake secrets.
%% - If server: use traffic secrets as by this time the client's state machine
%%              already stepped into the 'connection' state.
update_encryption_state(server, State0) ->
    State1 = calculate_traffic_secrets(State0),
    ssl_record:step_encryption_state(State1);
update_encryption_state(client, State) ->
    State.


validate_certificate_chain(CertEntries, CertDbHandle, CertDbRef,
                           SslOptions, CRLDbHandle, Role, Host, StaplingState) ->
    {Certs, ExtInfo} = split_cert_entries(CertEntries, StaplingState, [], #{}),
    ssl_handshake:certify(#certificate{asn1_certificates = Certs}, CertDbHandle,
                          CertDbRef, SslOptions, CRLDbHandle, Role, Host, ?TLS_1_3,
                          ExtInfo).

store_peer_cert(#state{session = Session,
                       handshake_env = HsEnv} = State, PeerCert, PublicKeyInfo) ->
    State#state{session = Session#session{peer_certificate = PeerCert},
                handshake_env = HsEnv#handshake_env{public_key_info = PublicKeyInfo}}.

split_cert_entries([], StaplingState, Chain, CertExt) ->
    {lists:reverse(Chain), #{cert_ext => CertExt,
                             stapling_state => StaplingState}};
split_cert_entries([#certificate_entry{data = DerCert,
                                       extensions = Extensions0} | CertEntries],
                   #{configured := StaplingConfigured} = StaplingState0, Chain,
                   CertExt) ->
    Id = public_key:pkix_subject_id(DerCert),
    Extensions = [ExtValue || {_, ExtValue} <- maps:to_list(Extensions0)],
    StaplingState = case {maps:get(status_request, Extensions0, undefined),
                          StaplingConfigured} of
                        {undefined, _} ->
                            StaplingState0;
                        {_, true} ->
                            StaplingState0#{status => received_staple}
                    end,
    split_cert_entries(CertEntries, StaplingState, [DerCert | Chain],
                       CertExt#{Id => Extensions}).

%% 4.4.1.  The Transcript Hash
%%
%% As an exception to this general rule, when the server responds to a
%% ClientHello with a HelloRetryRequest, the value of ClientHello1 is
%% replaced with a special synthetic handshake message of handshake type
%% "message_hash" containing Hash(ClientHello1).  I.e.,
%%
%% Transcript-Hash(ClientHello1, HelloRetryRequest, ... Mn) =
%%    Hash(message_hash ||        /* Handshake type */
%%         00 00 Hash.length  ||  /* Handshake message length (bytes) */
%%         Hash(ClientHello1) ||  /* Hash of ClientHello1 */
%%         HelloRetryRequest  || ... || Mn)
%%
%% NOTE: Hash.length is used in practice (openssl) and not message length!
%%       It is most probably a fault in the RFC.
replace_ch1_with_message_hash(#state{connection_states = ConnectionStates,
                                     handshake_env =
                                         #handshake_env{
                                            tls_handshake_history =
                                                {[HRR,CH1|HHistory], LM}} = HSEnv}  = State0) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,
    MessageHash = message_hash(CH1, HKDFAlgo),
    State0#state{handshake_env =
                     HSEnv#handshake_env{
                        tls_handshake_history =
                            {[HRR,MessageHash|HHistory], LM}}}.

get_hkdf_algorithm(ConnectionStates) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,
    HKDFAlgo.

message_hash(ClientHello1, HKDFAlgo) ->
    [?MESSAGE_HASH,
     0,0,ssl_cipher:hash_size(HKDFAlgo),
     crypto:hash(HKDFAlgo, ClientHello1)].


calculate_handshake_secrets(PublicKey, PrivateKey, SelectedGroup, PSK,
                              #state{connection_states = ConnectionStates,
                                     handshake_env =
                                         #handshake_env{
                                            tls_handshake_history = HHistory}} = State0) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo,
                         cipher_suite = CipherSuite} = SecParamsR,
    EarlySecret = tls_v1:key_schedule(early_secret, HKDFAlgo , {psk, PSK}),

    IKM = calculate_shared_secret(PublicKey, PrivateKey, SelectedGroup),
    HandshakeSecret = tls_v1:key_schedule(handshake_secret, HKDFAlgo, IKM, EarlySecret),

    %% Calculate [sender]_handshake_traffic_secret
    {Messages, _} =  HHistory,
    ClientHSTrafficSecret =
        tls_v1:client_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),
    ServerHSTrafficSecret =
        tls_v1:server_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),

    %% Calculate traffic keys
    KeyLength = tls_v1:key_length(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, KeyLength, ClientHSTrafficSecret),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, KeyLength, ServerHSTrafficSecret),

    %% Calculate Finished Keys
    ReadFinishedKey = tls_v1:finished_key(ClientHSTrafficSecret, HKDFAlgo),
    WriteFinishedKey = tls_v1:finished_key(ServerHSTrafficSecret, HKDFAlgo),

    State1 = maybe_store_handshake_traffic_secret(State0, ClientHSTrafficSecret, ServerHSTrafficSecret),

    update_pending_connection_states(State1, HandshakeSecret, undefined,
                                     undefined, undefined,
                                     ReadKey, ReadIV, ReadFinishedKey,
                                     WriteKey, WriteIV, WriteFinishedKey).

%% Server
calculate_client_early_traffic_secret(#state{connection_states = ConnectionStates,
                                             handshake_env =
                                                 #handshake_env{
                                                    tls_handshake_history = {Hist, _}}} = State, PSK) ->

    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{cipher_suite = CipherSuite} = SecParamsR,
    #{cipher := Cipher,
      prf := HKDF} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    calculate_client_early_traffic_secret(Hist, PSK, Cipher, HKDF, State).

%% Client
calculate_client_early_traffic_secret(
  ClientHello, PSK, Cipher, HKDFAlgo,
  #state{connection_states = ConnectionStates,
         ssl_options = Opts,
         static_env = #static_env{role = Role}} = State0) ->
    EarlySecret = tls_v1:key_schedule(early_secret, HKDFAlgo , {psk, PSK}),
    ClientEarlyTrafficSecret =
        tls_v1:client_early_traffic_secret(HKDFAlgo, EarlySecret, ClientHello),
    %% Calculate traffic key
    KeyLength = ssl_cipher:key_material(Cipher),
    {Key, IV} =
        tls_v1:calculate_traffic_keys(HKDFAlgo, KeyLength, ClientEarlyTrafficSecret),
    %% Update pending connection states
    case Role of
        client ->
            PendingWrite0 = ssl_record:pending_connection_state(ConnectionStates, write),
            PendingWrite1 = maybe_store_early_data_secret(Opts, ClientEarlyTrafficSecret,
                                                          PendingWrite0),
            PendingWrite = update_connection_state(PendingWrite1, undefined, undefined,
                                                   undefined,
                                                   Key, IV, undefined),
            State0#state{connection_states = ConnectionStates#{pending_write => PendingWrite}};
        server ->
            PendingRead0 = ssl_record:pending_connection_state(ConnectionStates, read),
            PendingRead1 = maybe_store_early_data_secret(Opts, ClientEarlyTrafficSecret,
                                                         PendingRead0),
            PendingRead = update_connection_state(PendingRead1, undefined, undefined,
                                                   undefined,
                                                   Key, IV, undefined),
            State0#state{connection_states = ConnectionStates#{pending_read => PendingRead}}
    end.



maybe_store_early_data_secret(#{keep_secrets := true}, EarlySecret, State) ->
    #{security_parameters := SecParams0} = State,
    SecParams = SecParams0#security_parameters{client_early_data_secret = EarlySecret},
    State#{security_parameters := SecParams};
maybe_store_early_data_secret(_, _, State) ->
    State.

%% Server
%% get_pre_shared_key(undefined, HKDFAlgo) ->
%%     binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo));
%% get_pre_shared_key(#pre_shared_key_server_hello{selected_identity = PSK}, _) ->
%%     PSK.
get_pre_shared_key(undefined, HKDFAlgo) ->
    binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo));
get_pre_shared_key({_, PSK}, _) ->
    PSK.
%%
%% Client
%% Server initiates a full handshake
get_pre_shared_key(_, _, HKDFAlgo, undefined) ->
    {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
%% Session resumption not configured
get_pre_shared_key(undefined, _, HKDFAlgo, _) ->
    {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
get_pre_shared_key(_, undefined, HKDFAlgo, _) ->
    {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
%% Session resumption
get_pre_shared_key(manual = SessionTickets, UseTicket, HKDFAlgo, ServerPSK) ->
    TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
    case choose_psk(TicketData, ServerPSK) of
        undefined -> %% full handshake, default PSK
            {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
        illegal_parameter ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
        {_, PSK, _, _, _} ->
            {ok, PSK}
    end;
get_pre_shared_key(auto = SessionTickets, UseTicket, HKDFAlgo, ServerPSK) ->
    TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
    case choose_psk(TicketData, ServerPSK) of
        undefined -> %% full handshake, default PSK
            tls_client_ticket_store:unlock_tickets(self(), UseTicket),
            {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
        illegal_parameter ->
            tls_client_ticket_store:unlock_tickets(self(), UseTicket),
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
        {Key, PSK, _, _, _} ->
            tls_client_ticket_store:remove_tickets([Key]),  %% Remove single-use ticket
            tls_client_ticket_store:unlock_tickets(self(), UseTicket -- [Key]),
            {ok, PSK}
    end.
%%
%% Early Data
get_pre_shared_key_early_data(SessionTickets, UseTicket) ->
    TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
    case choose_psk(TicketData,  #pre_shared_key_server_hello{selected_identity = 0}) of
        undefined -> %% Should not happen
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
        illegal_parameter ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
        {_Key, PSK, Cipher, HKDF, MaxSize} ->
            {ok, {PSK, Cipher, HKDF, MaxSize}}
    end.

get_supported_groups(undefined = Groups) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, {supported_groups, Groups})};
get_supported_groups(#supported_groups{supported_groups = Groups}) ->
    {ok, Groups}.

choose_psk(undefined, _) ->
    undefined;
choose_psk([], _) ->
    illegal_parameter;
choose_psk([#ticket_data{
               key = Key,
               pos = SelectedIdentity,
               psk = PSK,
               cipher_suite = {Cipher, HKDF},
               max_size = MaxSize}|_], #pre_shared_key_server_hello{selected_identity = SelectedIdentity}) ->
    {Key, PSK, Cipher, HKDF, MaxSize};
choose_psk([_|T], SelectedIdentity) ->
    choose_psk(T, SelectedIdentity).


calculate_traffic_secrets(#state{
                             static_env = #static_env{role = Role},
                             connection_states = ConnectionStates,
                             handshake_env =
                                 #handshake_env{
                                    tls_handshake_history = HHistory}} = State0) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo,
                         cipher_suite = CipherSuite,
                         master_secret = HandshakeSecret} = SecParamsR,

    MasterSecret =
        tls_v1:key_schedule(master_secret, HKDFAlgo, HandshakeSecret),

    %% Get the correct list messages for the handshake context.
    Messages = get_handshake_context(Role, HHistory),

    %% Calculate [sender]_application_traffic_secret_0
    ClientAppTrafficSecret0 =
        tls_v1:client_application_traffic_secret_0(HKDFAlgo, MasterSecret, lists:reverse(Messages)),
    ServerAppTrafficSecret0 =
        tls_v1:server_application_traffic_secret_0(HKDFAlgo, MasterSecret, lists:reverse(Messages)),

    %% Calculate traffic keys
    KeyLength = tls_v1:key_length(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, KeyLength, ClientAppTrafficSecret0),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, KeyLength, ServerAppTrafficSecret0),

    update_pending_connection_states(State0, MasterSecret, undefined,
                                     ClientAppTrafficSecret0, ServerAppTrafficSecret0,
                                     ReadKey, ReadIV, undefined,
                                     WriteKey, WriteIV, undefined).

%% X25519, X448
calculate_shared_secret(OthersKey, MyKey, Group)
  when is_binary(OthersKey) andalso is_binary(MyKey) andalso
       (Group =:= x25519 orelse Group =:= x448)->
    crypto:compute_key(ecdh, OthersKey, MyKey, Group);
%% FFDHE
calculate_shared_secret(OthersKey, MyKey, Group)
  when is_binary(OthersKey) andalso is_binary(MyKey) ->
    Params = #'DHParameter'{prime = P} = ssl_dh_groups:dh_params(Group),
    S = public_key:compute_key(OthersKey, MyKey, Params),
    Size = byte_size(binary:encode_unsigned(P)),
    ssl_cipher:add_zero_padding(S, Size);
%% ECDHE
calculate_shared_secret(OthersKey, MyKey = #'ECPrivateKey'{}, _Group)
  when is_binary(OthersKey) ->
    Point = #'ECPoint'{point = OthersKey},
    public_key:compute_key(Point, MyKey).


maybe_calculate_resumption_master_secret(#state{ssl_options = #{session_tickets := disabled}} = State) ->
    State;
maybe_calculate_resumption_master_secret(#state{
                             ssl_options = #{session_tickets := SessionTickets},
                             connection_states = ConnectionStates,
                             handshake_env =
                                                #handshake_env{
                                                   tls_handshake_history = HHistory}} = State)
  when SessionTickets =/= disabled  ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{master_secret = MasterSecret,
                         prf_algorithm = HKDFAlgo} = SecParamsR,
    {Messages0, _} = HHistory,
    RMS = tls_v1:resumption_master_secret(HKDFAlgo, MasterSecret, lists:reverse(Messages0)),
    update_resumption_master_secret(State, RMS).

calculate_exporter_master_secret(#state{
                             static_env = #static_env{role = Role},
                             connection_states = ConnectionStates,
                             handshake_env =
                                 #handshake_env{
                                    tls_handshake_history = HHistory}}) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo,
                         master_secret = MasterSecret} = SecParamsR,
    Messages = get_handshake_context(Role, HHistory),
    tls_v1:exporter_master_secret(HKDFAlgo, MasterSecret, lists:reverse(Messages)).

forget_master_secret(#state{connection_states =
                                #{pending_read := PendingRead,
                                  pending_write := PendingWrite,
                                  current_read := CurrentRead,
                                  current_write := CurrentWrite} = CS} = State) ->
    State#state{connection_states = CS#{pending_read => overwrite_master_secret(PendingRead),
                                        pending_write => overwrite_master_secret(PendingWrite),
                                        current_read => overwrite_master_secret(CurrentRead),
                                        current_write => overwrite_master_secret(CurrentWrite)}}.


overwrite_master_secret(ConnectionState = #{security_parameters := SecurityParameters0}) ->
    SecurityParameters = SecurityParameters0#security_parameters{master_secret = {master_secret, <<0>>}},
    ConnectionState#{security_parameters => SecurityParameters}.


set_client_random(#state{connection_states =
                             #{pending_read := PendingRead,
                               pending_write := PendingWrite,
                               current_read := CurrentRead,
                               current_write := CurrentWrite} = CS} = State, ClientRandom) ->
    State#state{connection_states = CS#{pending_read => overwrite_client_random(PendingRead, ClientRandom),
                                        pending_write => overwrite_client_random(PendingWrite, ClientRandom),
                                        current_read => overwrite_client_random(CurrentRead, ClientRandom),
                                        current_write => overwrite_client_random(CurrentWrite, ClientRandom)}}.


overwrite_client_random(ConnectionState = #{security_parameters := SecurityParameters0}, ClientRandom) ->
    SecurityParameters = SecurityParameters0#security_parameters{client_random = ClientRandom},
    ConnectionState#{security_parameters => SecurityParameters}.


maybe_store_handshake_traffic_secret(#state{connection_states =
                                                #{pending_read := PendingRead} = CS,
                                            ssl_options = #{keep_secrets := true}} = State,
                                     ClientHSTrafficSecret, ServerHSTrafficSecret) ->
    PendingRead1 = store_handshake_traffic_secret(PendingRead, ClientHSTrafficSecret, ServerHSTrafficSecret),
    State#state{connection_states = CS#{pending_read => PendingRead1}};
maybe_store_handshake_traffic_secret(State, _, _) ->
    State.

store_handshake_traffic_secret(ConnectionState, ClientHSTrafficSecret, ServerHSTrafficSecret) ->
    ConnectionState#{client_handshake_traffic_secret => ClientHSTrafficSecret,
                     server_handshake_traffic_secret => ServerHSTrafficSecret}.


update_pending_connection_states(#state{
                                    static_env = #static_env{role = server},
                                    connection_states =
                                        CS = #{pending_read := PendingRead0,
                                               pending_write := PendingWrite0}} = State,
                                 HandshakeSecret, ResumptionMasterSecret,
                                 ClientAppTrafficSecret, ServerAppTrafficSecret,
                                 ReadKey, ReadIV, ReadFinishedKey,
                                 WriteKey, WriteIV, WriteFinishedKey) ->
    PendingRead = update_connection_state(PendingRead0, HandshakeSecret, ResumptionMasterSecret,
                                          ClientAppTrafficSecret,
                                          ReadKey, ReadIV, ReadFinishedKey),
    PendingWrite = update_connection_state(PendingWrite0, HandshakeSecret, ResumptionMasterSecret,
                                           ServerAppTrafficSecret,
                                           WriteKey, WriteIV, WriteFinishedKey),
    State#state{connection_states = CS#{pending_read => PendingRead,
                                        pending_write => PendingWrite}};
update_pending_connection_states(#state{
                                    static_env = #static_env{role = client},
                                    connection_states =
                                        CS = #{pending_read := PendingRead0,
                                               pending_write := PendingWrite0}} = State,
                                 HandshakeSecret, ResumptionMasterSecret,
                                 ClientAppTrafficSecret, ServerAppTrafficSecret,
                                 ReadKey, ReadIV, ReadFinishedKey,
                                 WriteKey, WriteIV, WriteFinishedKey) ->
    PendingRead = update_connection_state(PendingRead0, HandshakeSecret, ResumptionMasterSecret,
                                          ServerAppTrafficSecret,
                                          WriteKey, WriteIV, WriteFinishedKey),
    PendingWrite = update_connection_state(PendingWrite0, HandshakeSecret, ResumptionMasterSecret,
                                           ClientAppTrafficSecret,
                                           ReadKey, ReadIV, ReadFinishedKey),
    State#state{connection_states = CS#{pending_read => PendingRead,
                                        pending_write => PendingWrite}}.


update_connection_state(ConnectionState = #{security_parameters := SecurityParameters0},
                        HandshakeSecret, ResumptionMasterSecret,
                        ApplicationTrafficSecret, Key, IV, FinishedKey) ->
    %% Store secret
    SecurityParameters = SecurityParameters0#security_parameters{
                           master_secret = HandshakeSecret,
                           resumption_master_secret = ResumptionMasterSecret,
                           application_traffic_secret = ApplicationTrafficSecret},
    BulkCipherAlgo = SecurityParameters#security_parameters.bulk_cipher_algorithm,
    ConnectionState#{security_parameters => SecurityParameters,
                     cipher_state => cipher_init(BulkCipherAlgo, Key, IV, FinishedKey)}.


update_start_state(State, Map) ->
    Cipher = maps:get(cipher, Map, undefined),
    KeyShare = maps:get(key_share, Map, undefined),
    SessionId = maps:get(session_id, Map, undefined),
    Group = maps:get(group, Map, undefined),
    SelectedSignAlg = maps:get(sign_alg, Map, undefined),
    PeerPublicKey = maps:get(peer_public_key, Map, undefined),
    ALPNProtocol = maps:get(alpn, Map, undefined),
    Random = maps:get(random, Map),
    update_start_state(State, Cipher, KeyShare, SessionId,
                       Group, SelectedSignAlg, PeerPublicKey,
                       ALPNProtocol, Random).
%%
update_start_state(#state{connection_states = ConnectionStates0,
                          handshake_env = #handshake_env{} = HsEnv,
                          static_env = #static_env{role = Role},
                          connection_env = CEnv,
                          session = Session} = State,
                   Cipher, KeyShare, SessionId,
                   Group, SelectedSignAlg, PeerPublicKey, ALPNProtocol, Random) ->
    #{security_parameters := SecParamsR0} = PendingRead =
        maps:get(pending_read, ConnectionStates0),
    #{security_parameters := SecParamsW0} = PendingWrite =
        maps:get(pending_write, ConnectionStates0),
    SecParamsR1 = ssl_cipher:security_parameters_1_3(SecParamsR0, Cipher),
    SecParamsW1 = ssl_cipher:security_parameters_1_3(SecParamsW0, Cipher),

    SecParamsR = update_random(Role, SecParamsR1, Random),
    SecParamsW = update_random(Role, SecParamsW1, Random),

    ConnectionStates =
        ConnectionStates0#{pending_read => PendingRead#{security_parameters => SecParamsR},
                           pending_write => PendingWrite#{security_parameters => SecParamsW}},
    State#state{connection_states = ConnectionStates,
                handshake_env = HsEnv#handshake_env{alpn = ALPNProtocol},
                key_share = KeyShare,
                session = Session#session{session_id = SessionId,
                                          ecc = Group,
                                          sign_alg = SelectedSignAlg,
                                          dh_public_value = PeerPublicKey,
                                          cipher_suite = Cipher},
                connection_env = CEnv#connection_env{negotiated_version = ?TLS_1_3}}.

update_random(server, SParams, Random) ->
    SParams#security_parameters{client_random = Random};
update_random(client, SParams, Random) ->
    SParams#security_parameters{server_random = Random}.

update_resumption_master_secret(#state{connection_states = ConnectionStates0} = State,
                                ResumptionMasterSecret) ->
    #{security_parameters := SecParamsR0} = PendingRead =
        maps:get(pending_read, ConnectionStates0),
    #{security_parameters := SecParamsW0} = PendingWrite =
        maps:get(pending_write, ConnectionStates0),

    SecParamsR = SecParamsR0#security_parameters{resumption_master_secret = ResumptionMasterSecret},
    SecParamsW = SecParamsW0#security_parameters{resumption_master_secret = ResumptionMasterSecret},
    ConnectionStates =
        ConnectionStates0#{pending_read => PendingRead#{security_parameters => SecParamsR},
                           pending_write => PendingWrite#{security_parameters => SecParamsW}},
    State#state{connection_states = ConnectionStates}.


cipher_init(?AES_CCM_8, Key, IV, FinishedKey) ->
    #cipher_state{key = Key,
                  iv = IV,
                  finished_key = FinishedKey,
                  tag_len = 8};
cipher_init(_BulkCipherAlgo, Key, IV, FinishedKey) ->
    #cipher_state{key = Key,
                  iv = IV,
                  finished_key = FinishedKey,
                  tag_len = 16}.


%% Get handshake context for verification of CertificateVerify.
%%
%% Verify CertificateVerify:
%%    ClientHello         (client) (1)
%%    ServerHello         (server) (2)
%%    EncryptedExtensions (server) (8)
%%    CertificateRequest  (server) (13)
%%    Certificate         (server) (11)
%%    CertificateVerify   (server) (15)
%%    Finished            (server) (20)
%%    Certificate         (client) (11)
%%    CertificateVerify   (client) (15) - Drop! Not included in calculations!
get_handshake_context_cv({[<<15,_/binary>>|Messages], _}) ->
    Messages.


%% Get handshake context for traffic key calculation.
%%
%% Client is authenticated with certificate:
%%    ClientHello         (client) (1)
%%    ServerHello         (server) (2)
%%    EncryptedExtensions (server) (8)
%%    CertificateRequest  (server) (13)
%%    Certificate         (server) (11)
%%    CertificateVerify   (server) (15)
%%    Finished            (server) (20)
%%    Certificate         (client) (11) - Drop! Not included in calculations!
%%    CertificateVerify   (client) (15) - Drop! Not included in calculations!
%%    Finished            (client) (20) - Drop! Not included in calculations!
%%
%% Client is authenticated but sends empty certificate:
%%    ClientHello         (client) (1)
%%    ServerHello         (server) (2)
%%    EncryptedExtensions (server) (8)
%%    CertificateRequest  (server) (13)
%%    Certificate         (server) (11)
%%    CertificateVerify   (server) (15)
%%    Finished            (server) (20)
%%    Certificate         (client) (11) - Drop! Not included in calculations!
%%    Finished            (client) (20) - Drop! Not included in calculations!
%%
%% Client is not authenticated:
%%    ClientHello         (client) (1)
%%    ServerHello         (server) (2)
%%    EncryptedExtensions (server) (8)
%%    Certificate         (server) (11)
%%    CertificateVerify   (server) (15)
%%    Finished            (server) (20)
%%    Finished            (client) (20) - Drop! Not included in calculations!
%%
%% Drop all client messages from the front of the iolist using the property that
%% incoming messages are binaries.
get_handshake_context(server, {Messages, _}) ->
    get_handshake_context_server(Messages);
get_handshake_context(client, {Messages, _}) ->
    get_handshake_context_client(Messages).

get_handshake_context_server([H|T]) when is_binary(H) ->
    get_handshake_context_server(T);
get_handshake_context_server(L) ->
    L.


get_handshake_context_client([H|T]) when is_list(H) ->
    get_handshake_context_client(T);
get_handshake_context_client(L) ->
    L.


%% If the CertificateVerify message is sent by a server, the signature
%% algorithm MUST be one offered in the client's "signature_algorithms"
%% extension unless no valid certificate chain can be produced without
%% unsupported algorithms
%%
%% If sent by a client, the signature algorithm used in the signature
%% MUST be one of those present in the supported_signature_algorithms
%% field of the "signature_algorithms" extension in the
%% CertificateRequest message.
verify_signature_algorithm(#state{
                              static_env = #static_env{role = Role},
                              ssl_options = #{signature_algs := LocalSignAlgs}} = State0,
                           #certificate_verify_1_3{algorithm = PeerSignAlg}) ->
    case lists:member(PeerSignAlg, filter_tls13_algs(LocalSignAlgs)) of
        true ->
            {ok, maybe_update_selected_sign_alg(State0, PeerSignAlg, Role)};
        false ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,
                                "CertificateVerify uses unsupported signature algorithm"), State}}
    end.


maybe_update_selected_sign_alg(#state{session = Session} = State, SignAlg, client) ->
    State#state{session = Session#session{sign_alg = SignAlg}};
maybe_update_selected_sign_alg(State, _, _) ->
    State.





context_string(server) ->
    <<"TLS 1.3, server CertificateVerify">>;
context_string(client) ->
    <<"TLS 1.3, client CertificateVerify">>.


%% Return context string for verifying peer signature
peer_context_string(server) ->
    <<"TLS 1.3, client CertificateVerify">>;
peer_context_string(client) ->
    <<"TLS 1.3, server CertificateVerify">>.


%% If there is no overlap between the received
%% "supported_groups" and the groups supported by the server, then the
%% server MUST abort the handshake with a "handshake_failure" or an
%% "insufficient_security" alert.
select_common_groups(_, []) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_groups)};
select_common_groups(ServerGroups, ClientGroups) ->
    Fun = fun(E) -> lists:member(E, ClientGroups) end,
    case lists:filter(Fun, ServerGroups) of
        [] ->
            select_common_groups(ServerGroups, []);
        L ->
            {ok, L}
    end.

%% RFC 8446 (TLS 1.3)
%% TLS 1.3 provides two extensions for indicating which signature
%% algorithms may be used in digital signatures.  The
%% "signature_algorithms_cert" extension applies to signatures in
%% certificates and the "signature_algorithms" extension, which
%% originally appeared in TLS 1.2, applies to signatures in
%% CertificateVerify messages.
%%
%% If no "signature_algorithms_cert" extension is
%% present, then the "signature_algorithms" extension also applies to
%% signatures appearing in certificates.

%% Check if the signature algorithm of the server certificate is supported
%% by the client.
check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgs, undefined) ->
    do_check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgs);
check_cert_sign_algo(SignAlgo, SignHash, _, ClientSignAlgsCert) ->
    do_check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgsCert).


%% DSA keys are not supported by TLS 1.3
select_sign_algo(dsa, _RSAKeySize, _CertSignAlg, _OwnSignAlgs, _Curve) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key)};
select_sign_algo(_, _RSAKeySize, [], _, _) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)};
select_sign_algo(_, _RSAKeySize, undefined, _OwnSignAlgs, _) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key)};
select_sign_algo(PublicKeyAlgo, RSAKeySize, [CertSignAlg|CertSignAlgs], OwnSignAlgs, Curve) ->
    {_, S, _} = ssl_cipher:scheme_to_components(CertSignAlg),
    %% RSASSA-PKCS1-v1_5 and Legacy algorithms are not defined for use in signed
    %% TLS handshake messages: filter sha-1 and rsa_pkcs1.
    %%
    %% RSASSA-PSS RSAE algorithms: If the public key is carried in an X.509
    %% certificate, it MUST use the rsaEncryption OID.
    %% RSASSA-PSS PSS algorithms: If the public key is carried in an X.509 certificate,
    %% it MUST use the RSASSA-PSS OID.
    case ((PublicKeyAlgo =:= rsa andalso S =:= rsa_pss_rsae)
          orelse (PublicKeyAlgo =:= rsa_pss_pss andalso S =:= rsa_pss_pss)
          orelse (PublicKeyAlgo =:= ecdsa andalso S =:= ecdsa)
          orelse (PublicKeyAlgo =:= eddsa andalso S =:= eddsa)
         )
        andalso
        lists:member(CertSignAlg, OwnSignAlgs) of
        true ->
            validate_key_compatibility(PublicKeyAlgo, RSAKeySize,
                                       [CertSignAlg|CertSignAlgs], OwnSignAlgs, Curve);
        false ->
            select_sign_algo(PublicKeyAlgo, RSAKeySize, CertSignAlgs, OwnSignAlgs, Curve)
    end.

validate_key_compatibility(PublicKeyAlgo, RSAKeySize, [CertSignAlg|CertSignAlgs], OwnSignAlgs, Curve)
  when PublicKeyAlgo =:= rsa orelse
       PublicKeyAlgo =:= rsa_pss_pss ->
    {Hash, Sign, _} = ssl_cipher:scheme_to_components(CertSignAlg),
    case (Sign =:= rsa_pss_rsae orelse Sign =:= rsa_pss_pss) andalso
        is_rsa_key_compatible(RSAKeySize, Hash) of
        true ->
            {ok, CertSignAlg};
        false ->
            select_sign_algo(PublicKeyAlgo, RSAKeySize, CertSignAlgs, OwnSignAlgs, Curve)
    end;
validate_key_compatibility(PublicKeyAlgo, RSAKeySize, [CertSignAlg|CertSignAlgs], OwnSignAlgs, Curve)
  when PublicKeyAlgo =:= ecdsa ->
    {_ , Sign, PeerCurve} = ssl_cipher:scheme_to_components(CertSignAlg),
    case Sign =:= ecdsa andalso Curve =:= PeerCurve of
        true ->
            {ok, CertSignAlg};
        false ->
            select_sign_algo(PublicKeyAlgo, RSAKeySize, CertSignAlgs, OwnSignAlgs, Curve)
    end;
validate_key_compatibility(_, _, [CertSignAlg|_], _, _) ->
    {ok, CertSignAlg}.

is_rsa_key_compatible(KeySize, Hash) ->
    HashSize = ssl_cipher:hash_size(Hash),

    %% OpenSSL crypto lib defines a limit on the size of the random salt
    %% in PSS signatures based on the size of signing RSA key.
    %% If the limit is unchecked, it causes handshake failures when the
    %% configured certificates contain short (e.g. 1024-bit) RSA keys.
    %% For more information see the OpenSSL crypto library
    %% (rsa_pss:c{77,86}).
    %% TODO: Move this check into crypto. Investigate if this is a bug in
    %% OpenSSL crypto lib.
    if (KeySize < (HashSize + 2)) ->
            false;
       (HashSize > (KeySize - HashSize - 2)) ->
            false;
       true ->
            true
    end.

do_check_cert_sign_algo(_, _, undefined) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)};
do_check_cert_sign_algo(_, _, []) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)};
do_check_cert_sign_algo(SignAlgo, SignHash, [Scheme|T]) ->
    %% ECDSA: curve is tied to the hash algorithm e.g. ecdsa_secp256r1_sha256
    {Hash, Sign, _Curve} = ssl_cipher:scheme_to_components(Scheme),
    case compare_sign_algos(SignAlgo, SignHash, Sign, Hash) of
        true ->
            ok;
        _Else ->
            do_check_cert_sign_algo(SignAlgo, SignHash, T)
    end.


%% id-RSASSA-PSS (rsa_pss) indicates that the key may only be used for PSS signatures.
compare_sign_algos(rsa_pss_pss, Hash, rsa_pss_pss, Hash) ->
     true;
%% rsaEncryption (rsa) allows the key to be used for any of the standard encryption or
%% signature schemes.
compare_sign_algos(rsa, Hash, Algo, Hash)
  when Algo =:= rsa_pss_rsae orelse
       Algo =:= rsa_pkcs1 ->
    true;
compare_sign_algos(Algo, Hash, Algo, Hash) ->
    true;
compare_sign_algos(_, _, _, _) ->
    false.

get_certificate_params(Cert) ->
    {SignAlgo0, Param, SubjectPublicKeyAlgo0, RSAKeySize, Curve} =
        ssl_handshake:get_cert_params(Cert),
    {SignHash, SignAlgo} = oids_to_atoms(SignAlgo0, Param),
    SubjectPublicKeyAlgo = public_key_algo(SubjectPublicKeyAlgo0),
    {SubjectPublicKeyAlgo, SignAlgo, SignHash, RSAKeySize, Curve}.

oids_to_atoms(?'id-RSASSA-PSS', #'RSASSA-PSS-params'{maskGenAlgorithm =
                                                        #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                            parameters = #'HashAlgorithm'{algorithm = HashOid}}}) ->
    Hash = public_key:pkix_hash_type(HashOid),
    {Hash, rsa_pss_pss};
oids_to_atoms(SignAlgo, _) ->
    public_key:pkix_sign_types(SignAlgo).

%% Note: copied from ssl_handshake
public_key_algo(?'id-RSASSA-PSS') ->
    rsa_pss_pss;
public_key_algo(?rsaEncryption) ->
    rsa;
public_key_algo(?'id-ecPublicKey') ->
    ecdsa;
public_key_algo(?'id-Ed25519') ->
    eddsa;
public_key_algo(?'id-Ed448') ->
    eddsa;
public_key_algo(?'id-dsa') ->
    dsa.

get_signature_scheme_list(undefined) ->
    undefined;
get_signature_scheme_list(#hash_sign_algos{}) ->
    [];
get_signature_scheme_list(#signature_algorithms_cert{
                        signature_scheme_list = ClientSignatureSchemes}) ->
    ClientSignatureSchemes;
get_signature_scheme_list(#signature_algorithms{
                        signature_scheme_list = ClientSignatureSchemes}) ->
    %% Filter unassigned and legacy elements
    lists:filter(fun (E) -> is_atom(E) andalso E =/= unassigned end,
                 ClientSignatureSchemes).

get_certificate_authorities(#certificate_authorities{authorities = Auths}) ->
    Auths;
get_certificate_authorities(undefined) ->
    [].



%% Prior to accepting PSK key establishment, the server MUST validate
%% the corresponding binder value (see Section 4.2.11.2 below).  If this
%% value is not present or does not validate, the server MUST abort the
%% handshake.  Servers SHOULD NOT attempt to validate multiple binders;
%% rather, they SHOULD select a single PSK and validate solely the
%% binder that corresponds to that PSK.
%%
%% If no acceptable PSKs are found, the server SHOULD perform a non-PSK
%% handshake if possible.
handle_pre_shared_key(_, undefined, _) ->
    {ok, undefined};
handle_pre_shared_key(#state{ssl_options = #{session_tickets := disabled}}, _, _) ->
    {ok, undefined};
handle_pre_shared_key(#state{ssl_options = #{session_tickets := Tickets},
                             handshake_env = #handshake_env{tls_handshake_history =  {HHistory, _}},
                             static_env = #static_env{trackers = Trackers}},
                      #pre_shared_key_client_hello{offered_psks =
                                                       OfferedPreSharedKeys}, Cipher) when Tickets =/= disabled ->
    Tracker = proplists:get_value(session_tickets_tracker, Trackers),
    #{prf := CipherHash} = ssl_cipher_format:suite_bin_to_map(Cipher),
    tls_server_session_ticket:use(Tracker, OfferedPreSharedKeys, CipherHash, HHistory).

%% If the handshake includes a HelloRetryRequest, the initial
%% ClientHello and HelloRetryRequest are included in the transcript
%% along with the new ClientHello.  For instance, if the client sends
%% ClientHello1, its binder will be computed over:
%%
%%    Transcript-Hash(Truncate(ClientHello1))
%%
%% Where Truncate() removes the binders list from the ClientHello.
%%
%% If the server responds with a HelloRetryRequest and the client then
%% sends ClientHello2, its binder will be computed over:
%%
%%    Transcript-Hash(ClientHello1,
%%                    HelloRetryRequest,
%%                    Truncate(ClientHello2))
%%
%% The full ClientHello1/ClientHello2 is included in all other handshake
%% hash computations.  Note that in the first flight,
%% Truncate(ClientHello1) is hashed directly, but in the second flight,
%% ClientHello1 is hashed and then reinjected as a "message_hash"
%% message, as described in Section 4.4.1.
maybe_add_binders(Hello, undefined, _) ->
    Hello;
maybe_add_binders(Hello0, TicketData, ?TLS_1_3=Version) ->
    HelloBin0 = tls_handshake:encode_handshake(Hello0, Version),
    HelloBin1 = iolist_to_binary(HelloBin0),
    Truncated = truncate_client_hello(HelloBin1),
    Binders = create_binders([Truncated], TicketData),
    update_binders(Hello0, Binders);
maybe_add_binders(Hello, _, Version) when ?TLS_LTE(Version, ?TLS_1_2) ->
    Hello.
%%
%% HelloRetryRequest
maybe_add_binders(Hello, _, undefined, _) ->
    Hello;
maybe_add_binders(Hello0, {[HRR,MessageHash|_], _}, TicketData, ?TLS_1_3=Version) ->
    HelloBin0 = tls_handshake:encode_handshake(Hello0, Version),
    HelloBin1 = iolist_to_binary(HelloBin0),
    Truncated = truncate_client_hello(HelloBin1),
    Binders = create_binders([MessageHash,HRR,Truncated], TicketData),
    update_binders(Hello0, Binders);
maybe_add_binders(Hello, _, _, Version) when ?TLS_LTE(Version, ?TLS_1_2) ->
    Hello.

create_binders(Context, TicketData) ->
    create_binders(Context, TicketData, []).
%%
create_binders(_, [], Acc) ->
    lists:reverse(Acc);
create_binders(Context, [#ticket_data{
                            psk = PSK,
                            cipher_suite = {_, HKDF}}|T], Acc) ->
    FinishedKey = calculate_finished_key(PSK, HKDF),
    Binder = calculate_binder(FinishedKey, HKDF, Context),
    create_binders(Context, T, [Binder|Acc]).

%% Removes the binders list from the ClientHello.
%% opaque PskBinderEntry<32..255>;
%%
%% struct {
%%     PskIdentity identities<7..2^16-1>;
%%     PskBinderEntry binders<33..2^16-1>;
%% } OfferedPsks;
truncate_client_hello(HelloBin0) ->
    <<?BYTE(Type), ?UINT24(_Length), Body/binary>> = HelloBin0,
    CH0 = #client_hello{
             extensions = #{pre_shared_key := PSK0} = Extensions0} =
        tls_handshake:decode_handshake(?TLS_1_3, Type, Body),
    #pre_shared_key_client_hello{offered_psks = OfferedPsks0} = PSK0,
    OfferedPsks = OfferedPsks0#offered_psks{binders = []},
    PSK = PSK0#pre_shared_key_client_hello{offered_psks = OfferedPsks},
    Extensions = Extensions0#{pre_shared_key => PSK},
    CH = CH0#client_hello{extensions = Extensions},

    %% Decoding a ClientHello from an another TLS implementation can contain
    %% unsupported extensions and thus executing decoding and encoding on
    %% the input can result in a different handshake binary.
    %% The original length of the binders can still be determined by
    %% re-encoding the original ClientHello and using its size as reference
    %% when we subtract the size of the truncated binary.
    TruncatedSize = iolist_size(tls_handshake:encode_handshake(CH, ?TLS_1_3)),
    RefSize = iolist_size(tls_handshake:encode_handshake(CH0, ?TLS_1_3)),
    BindersSize = RefSize - TruncatedSize,

    %% Return the truncated ClientHello by cutting of the binders from the original
    %% ClientHello binary.
    {Truncated, _} = split_binary(HelloBin0, byte_size(HelloBin0) - BindersSize - 2),
    Truncated.

maybe_add_early_data_indication(#client_hello{
                                   extensions = Extensions0} = ClientHello,
                                EarlyData,
                                ?TLS_1_3)
  when is_binary(EarlyData) andalso
       byte_size(EarlyData) > 0 ->
    Extensions = Extensions0#{early_data =>
                                  #early_data_indication{}},
    ClientHello#client_hello{extensions = Extensions};
maybe_add_early_data_indication(ClientHello, _, _) ->
    ClientHello.

supported_groups_from_extensions(Extensions) ->
    case maps:get(elliptic_curves, Extensions, undefined) of
        #supported_groups{} = Groups->
            {ok, Groups};
        %% We do not support legacy for TLS-1.2 in TLS-1.3
        #elliptic_curves{} ->
           {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
        undefined ->
            {ok, undefined}
    end.

%% The PskBinderEntry is computed in the same way as the Finished
%% message (Section 4.4.4) but with the BaseKey being the binder_key
%% derived via the key schedule from the corresponding PSK which is
%% being offered (see Section 7.1).
calculate_finished_key(PSK, HKDFAlgo) ->
    EarlySecret = tls_v1:key_schedule(early_secret, HKDFAlgo , {psk, PSK}),
    PRK = tls_v1:resumption_binder_key(HKDFAlgo, EarlySecret),
    tls_v1:finished_key(PRK, HKDFAlgo).


calculate_binder(BinderKey, HKDF, Truncated) ->
    tls_v1:finished_verify_data(BinderKey, HKDF, [Truncated]).


update_binders(#client_hello{extensions =
                                 #{pre_shared_key := PreSharedKey0} = Extensions0} = Hello, Binders) ->
    #pre_shared_key_client_hello{
       offered_psks =
           #offered_psks{identities = Identities}} = PreSharedKey0,

    PreSharedKey =
        #pre_shared_key_client_hello{
           offered_psks =
               #offered_psks{identities = Identities,
                             binders = Binders}},

    Extensions = Extensions0#{pre_shared_key => PreSharedKey},
    Hello#client_hello{extensions = Extensions}.


early_data_size(undefined) ->
    undefined;
early_data_size(EarlyData) when is_binary(EarlyData) ->
    byte_size(EarlyData).

%% Choose a ticket based on the intention of the user. The first argument is
%% a 2-tuple of ticket keys where the first element refers to a ticket that
%% fulfills all criteria for sending early_data (hash, cipher, early data size).
%% Second argument refers to a ticket that can only be used for session
%% resumption.
choose_ticket({Key, _}, _) when Key =/= undefined ->
    Key;
choose_ticket({_, Key}, EarlyData) when EarlyData =:= undefined ->
    Key;
choose_ticket(_, _) ->
    %% No tickets found that fulfills the original intention of the user
    %% (sending early_data). It is possible to do session resumption but
    %% in that case the configured early data would have to be removed
    %% and that would contradict the will of the user. Returning undefined
    %% here prevents session resumption instead.
    undefined.

ciphers_for_early_data(CipherSuites0) ->
    %% Return supported block cipher algorithms
    lists:filtermap(fun ciphers_for_early_data0/1, CipherSuites0).

ciphers_for_early_data0(CipherSuite) ->
    %% Use only supported TLS 1.3 cipher suites
    case lists:member(CipherSuite, tls_v1:exclusive_suites(?TLS_1_3)) of
        true -> {true, maps:get(cipher, ssl_cipher_format:suite_bin_to_map(CipherSuite))};
        false -> false
    end.


get_ticket_data(_, undefined, _) ->
    undefined;
get_ticket_data(_, _, undefined) ->
    undefined;
get_ticket_data(_, manual, UseTicket) ->
    process_user_tickets(UseTicket);
get_ticket_data(Pid, auto, UseTicket) ->
    tls_client_ticket_store:get_tickets(Pid, UseTicket).


process_user_tickets(UseTicket) ->
    process_user_tickets(UseTicket, [], 0).
%%
process_user_tickets([], Acc, _) ->
    lists:reverse(Acc);
process_user_tickets([H|T], Acc, N) ->
    case process_ticket(H, N) of
        error ->
            process_user_tickets(T, Acc, N + 1);
        TicketData ->
            process_user_tickets(T, [TicketData|Acc], N + 1)
    end.

%% Used when session_tickets = manual
process_ticket(#{cipher_suite := CipherSuite,
                 sni := _SNI,  %% TODO user's responsibility to handle SNI?
                 psk := PSK,
                 timestamp := Timestamp,
                 ticket := NewSessionTicket}, N) ->
    #new_session_ticket{
       ticket_lifetime = _LifeTime,
       ticket_age_add = AgeAdd,
       ticket_nonce = Nonce,
       ticket = Ticket,
       extensions = Extensions
      } = NewSessionTicket,
    TicketAge = erlang:system_time(millisecond) - Timestamp,
    ObfuscatedTicketAge = obfuscate_ticket_age(TicketAge, AgeAdd),
    Identity = #psk_identity{
                  identity = Ticket,
                  obfuscated_ticket_age = ObfuscatedTicketAge},
    MaxEarlyData = get_max_early_data(Extensions),
    #ticket_data{
       key = undefined,
       pos = N,
       identity = Identity,
       psk = PSK,
       nonce = Nonce,
       cipher_suite = CipherSuite,
       max_size = MaxEarlyData};
process_ticket(_, _) ->
    error.

get_max_early_data(Extensions) ->
    EarlyDataIndication = maps:get(early_data, Extensions, undefined),
    case EarlyDataIndication of
        undefined ->
            undefined;
        #early_data_indication_nst{indication = MaxSize} ->
            MaxSize
    end.

%% The "obfuscated_ticket_age"
%% field of each PskIdentity contains an obfuscated version of the
%% ticket age formed by taking the age in milliseconds and adding the
%% "ticket_age_add" value that was included with the ticket
%% (see Section 4.6.1), modulo 2^32.
obfuscate_ticket_age(TicketAge, AgeAdd) ->
    (TicketAge + AgeAdd) rem round(math:pow(2,32)).

%% Call basic path validation algorithm in public_key for TLS-1.3
path_validation(TrustedCert, Path, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle, Version,
                #{verify_fun := VerifyFun,
                  customize_hostname_check := CustomizeHostnameCheck,
                  crl_check := CrlCheck,
                  log_level := LogLevel,
                  signature_algs := SignAlgos,
                  signature_algs_cert := SignAlgosCert} = Opts,
                #{cert_ext := CertExt,
                  stapling_state := StaplingState}) ->
    ValidationFunAndState =
        ssl_handshake:validation_fun_and_state(VerifyFun,
                                               #{role => Role,
                                                 certdb => CertDbHandle,
                                                 certdb_ref => CertDbRef,
                                                 server_name => ServerName,
                                                 customize_hostname_check =>
                                                     CustomizeHostnameCheck,
                                                 crl_check => CrlCheck,
                                                 crl_db => CRLDbHandle,
                                                 signature_algs => filter_tls13_algs(SignAlgos),
                                                 signature_algs_cert =>
                                                     filter_tls13_algs(SignAlgosCert),
                                                 version => Version,
                                                 issuer => TrustedCert,
                                                 cert_ext => CertExt,
                                                 stapling_state => StaplingState,
                                                 path_len => length(Path)
                                                },
                                               Path, LogLevel),
    Options = [{max_path_length, maps:get(depth, Opts, ?DEFAULT_DEPTH)},
               {verify_fun, ValidationFunAndState}],
    public_key:pkix_path_validation(TrustedCert, Path, Options).

select_client_cert_key_pair(Session0,
                            [#{private_key := NoKey, certs := [[]] = NoCerts}],
                            _,_,_,_,_,_, _) ->
    %% No certificate supplied : send empty certificate
    Session0#session{own_certificates = NoCerts,
                     private_key = NoKey};
select_client_cert_key_pair(Session, [],_,_,_,_,_,_, undefined) ->
    %% No certificate compliant with supported algorithms and
    %% extensison : send empty certificate in state 'wait_finished'
    Session#session{own_certificates = [[]],
                    private_key = #{}};
select_client_cert_key_pair(_,[],_,_,_,_,_,_, #session{} = Plausible) ->
    %% If we do not find an alternative chain with a cert signed in auth_domain,
    %% but have a single cert without chain certs it might be verifiable by
    %% a server that has the means to recreate the chain
    Plausible;
select_client_cert_key_pair(Session0, [#{private_key := Key, certs := [Cert| _] = Certs} | Rest],
                            ServerSignAlgs, ServerSignAlgsCert,
                            ClientSignAlgs, CertDbHandle, CertDbRef,
                            CertAuths, Plausible0) ->
    {PublicKeyAlgo, SignAlgo, SignHash, MaybeRSAKeySize, Curve} = get_certificate_params(Cert),
    case select_sign_algo(PublicKeyAlgo, MaybeRSAKeySize, ServerSignAlgs, ClientSignAlgs, Curve) of
        {ok, SelectedSignAlg} ->
            %% Check if server supports signature algorithm of client certificate
            case check_cert_sign_algo(SignAlgo, SignHash, ServerSignAlgs, ServerSignAlgsCert) of
                ok ->
                    case ssl_certificate:handle_cert_auths(Certs, CertAuths, CertDbHandle, CertDbRef) of
                        {ok, EncodedChain} ->
                            Session0#session{sign_alg = SelectedSignAlg,
                                             own_certificates = EncodedChain,
                                             private_key = Key
                                            };
                        {error, EncodedChain, not_in_auth_domain} ->
                            Plausible = plausible_missing_chain(EncodedChain, Plausible0,
                                                                SelectedSignAlg, Key, Session0),
                            select_client_cert_key_pair(Session0, Rest, ServerSignAlgs, ServerSignAlgsCert,
                                                        ClientSignAlgs, CertDbHandle, CertDbRef, CertAuths,
                                                        Plausible)
                    end;
                _ ->
                    select_client_cert_key_pair(Session0, Rest, ServerSignAlgs, ServerSignAlgsCert, ClientSignAlgs,
                                                CertDbHandle, CertDbRef, CertAuths, Plausible0)
            end;
        {error, _} ->
            select_client_cert_key_pair(Session0, Rest, ServerSignAlgsCert, ServerSignAlgsCert, ClientSignAlgs,
                                        CertDbHandle, CertDbRef, CertAuths, Plausible0)
    end.

plausible_missing_chain([_] = EncodedChain, undefined, SignAlg, Key, Session0) ->
    Session0#session{sign_alg = SignAlg,
                     own_certificates = EncodedChain,
                     private_key = Key
                    };
plausible_missing_chain(_,Plausible,_,_,_) ->
    Plausible.
