%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% Purpose: Help funtions for handling the TLS 1.3 (specific parts of)
%%% TLS handshake protocol
%%----------------------------------------------------------------------

-module(tls_handshake_1_3).

-include("tls_handshake_1_3.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").
-include("ssl_connection.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Encode
-export([encode_handshake/1, decode_handshake/2]).

%% Handshake
-export([handle_client_hello/3]).

%% Create handshake messages
-export([certificate/5,
         certificate_verify/4,
         encrypted_extensions/0,
         server_hello/4]).

-export([do_negotiated/2,
         do_wait_finished/2]).

%%====================================================================
%% Create handshake messages
%%====================================================================

server_hello(SessionId, KeyShare, ConnectionStates, _Map) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    Extensions = server_hello_extensions(KeyShare),
    #server_hello{server_version = {3,3}, %% legacy_version
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 0, %% legacy attribute
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  extensions = Extensions
		 }.

server_hello_extensions(KeyShare) ->
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
    Extensions = #{server_hello_selected_version => SupportedVersions},
    ssl_handshake:add_server_share(Extensions, KeyShare).

%% TODO: implement support for encrypted_extensions
encrypted_extensions() ->
    #encrypted_extensions{
       extensions = #{}
      }.

%% TODO: use maybe monad for error handling!
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
certificate(OwnCert, CertDbHandle, CertDbRef, _CRContext, server) ->
    case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	{ok, _, Chain} ->
            CertList = chain_to_cert_list(Chain),
            %% If this message is in response to a CertificateRequest, the value of
            %% certificate_request_context in that message. Otherwise (in the case
            %%of server authentication), this field SHALL be zero length.
	    #certificate_1_3{
               certificate_request_context = <<>>,
               certificate_list = CertList};
	{error, Error} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {server_has_no_suitable_certificates, Error})
    end.


certificate_verify(PrivateKey, SignatureScheme,
                   #state{connection_states = ConnectionStates,
                          handshake_env =
                              #handshake_env{
                                 tls_handshake_history = {Messages, _}}}, server) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, write),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    {HashAlgo, _, _} =
        ssl_cipher:scheme_to_components(SignatureScheme),

    Context = lists:reverse(Messages),

    %% Transcript-Hash uses the HKDF hash function defined by the cipher suite.
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),

    %% Digital signatures use the hash function defined by the selected signature
    %% scheme.
    case digitally_sign(THash, <<"TLS 1.3, server CertificateVerify">>,
                        HashAlgo, PrivateKey) of
        {ok, Signature} ->
            {ok, #certificate_verify_1_3{
                    algorithm = SignatureScheme,
                    signature = Signature
                   }};
        {error, badarg} ->
            {error, badarg}

    end.


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
    BinExts = encode_extensions(Exts),
    {?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                            ?BYTE(Nonce), ?UINT16(TicketSize), Ticket/binary,
                            BinExts/binary>>};
encode_handshake(#end_of_early_data{}) ->
    {?END_OF_EARLY_DATA, <<>>};
encode_handshake(#key_update{request_update = Update}) ->
    {?KEY_UPDATE, <<?BYTE(Update)>>};
encode_handshake(HandshakeMsg) ->
    ssl_handshake:encode_handshake(HandshakeMsg, {3,4}).


%%====================================================================
%% Decode handshake
%%====================================================================

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
                                        ?BYTE(Nonce), ?UINT16(TicketSize), Ticket:TicketSize/binary,
                                        BinExts/binary>>) ->
    Exts = decode_extensions(BinExts, encrypted_extensions),
    #new_session_ticket{ticket_lifetime = LifeTime,  
                        ticket_age_add = Age,   
                        ticket_nonce = Nonce,     
                        ticket = Ticket,           
                        extensions = Exts};
decode_handshake(?END_OF_EARLY_DATA, _) ->
    #end_of_early_data{};
decode_handshake(?KEY_UPDATE, <<?BYTE(Update)>>) ->
    #key_update{request_update = Update};
decode_handshake(Tag, HandshakeMsg) ->
    ssl_handshake:decode_handshake({3,4}, Tag, HandshakeMsg).

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

decode_cert_entries(Entries) ->
    decode_cert_entries(Entries, []).

decode_cert_entries(<<>>, Acc) ->
    lists:reverse(Acc);
decode_cert_entries(<<?UINT24(DSize), Data:DSize/binary, ?UINT16(Esize), BinExts:Esize/binary,
                      Rest/binary>>, Acc) ->
    Exts = decode_extensions(BinExts, certificate_request),
    decode_cert_entries(Rest, [#certificate_entry{data = Data,
                                                  extensions = Exts} | Acc]).

encode_extensions(Exts)->
    ssl_handshake:encode_extensions(extensions_list(Exts)).
decode_extensions(Exts, MessageType) ->
    ssl_handshake:decode_extensions(Exts, {3,4}, MessageType).

extensions_list(HelloExtensions) ->
    [Ext || {_, Ext} <- maps:to_list(HelloExtensions)].


%% TODO: add extensions!
chain_to_cert_list(L) ->
    chain_to_cert_list(L, []).
%%
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
digitally_sign(THash, Context, HashAlgo, PrivateKey) ->
    Content = build_content(Context, THash),

    %% The length of the Salt MUST be equal to the length of the output
    %% of the digest algorithm: rsa_pss_saltlen = -1
    try public_key:sign(Content, HashAlgo, PrivateKey,
                    [{rsa_padding, rsa_pkcs1_pss_padding},
                     {rsa_pss_saltlen, -1},
                     {rsa_mgf1_md, HashAlgo}]) of
        Signature ->
            {ok, Signature}
    catch
        error:badarg ->
            {error, badarg}
    end.


build_content(Context, THash) ->
    Prefix = binary:copy(<<32>>, 64),
    <<Prefix/binary,Context/binary,?BYTE(0),THash/binary>>.

%%====================================================================
%% Handle handshake messages
%%====================================================================

handle_client_hello(#client_hello{cipher_suites = ClientCiphers,
                                  session_id = SessionId,
                                  extensions = Extensions} = _Hello,
                    #ssl_options{ciphers = ServerCiphers,
                                 signature_algs = ServerSignAlgs,
                                 signature_algs_cert = _SignatureSchemes, %% TODO: Check??
                                 supported_groups = ServerGroups0} = _SslOpts,
                    Env) ->

    Cert = maps:get(cert, Env, undefined),

    ClientGroups0 = maps:get(elliptic_curves, Extensions, undefined),
    ClientGroups = get_supported_groups(ClientGroups0),
    ServerGroups = get_supported_groups(ServerGroups0),

    ClientShares0 = maps:get(key_share, Extensions, undefined),
    ClientShares = get_key_shares(ClientShares0),

    ClientSignAlgs = get_signature_scheme_list(
                       maps:get(signature_algs, Extensions, undefined)),
    ClientSignAlgsCert = get_signature_scheme_list(
                           maps:get(signature_algs_cert, Extensions, undefined)),

    %% TODO: use library function if it exists
    %% Init the maybe "monad"
    {Ref,Maybe} = maybe(),

    try
        %% If the server does not select a PSK, then the server independently selects a
        %% cipher suite, an (EC)DHE group and key share for key establishment,
        %% and a signature algorithm/certificate pair to authenticate itself to
        %% the client.
        Cipher = Maybe(select_cipher_suite(ClientCiphers, ServerCiphers)),
        Group = Maybe(select_server_group(ServerGroups, ClientGroups)),
        Maybe(validate_key_share(ClientGroups, ClientShares)),

        ClientPubKey = Maybe(get_client_public_key(Group, ClientShares)),

        {PublicKeyAlgo, SignAlgo, SignHash} = get_certificate_params(Cert),

        %% Check if client supports signature algorithm of server certificate
        Maybe(check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgs, ClientSignAlgsCert)),

        %% Select signature algorithm (used in CertificateVerify message).
        SelectedSignAlg = Maybe(select_sign_algo(PublicKeyAlgo, ClientSignAlgs, ServerSignAlgs)),

        %% Generate server_share
        KeyShare = ssl_cipher:generate_server_share(Group),
        _Ret = #{cipher => Cipher,
                group => Group,
                sign_alg => SelectedSignAlg,
                client_share => ClientPubKey,
                key_share => KeyShare,
                session_id => SessionId}

        %% TODO:
        %%   - session handling
        %%   - handle extensions: ALPN
        %%     (do not handle: NPN, srp, renegotiation_info, ec_point_formats)

    catch
        {Ref, {insufficient_security, no_suitable_groups}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_groups);
        {Ref, illegal_parameter} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
        {Ref, {hello_retry_request, _Group0}} ->
            %% TODO
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, "hello_retry_request not implemented");
        {Ref, no_suitable_cipher} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher);
        {Ref, {insufficient_security, no_suitable_signature_algorithm}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
        {Ref, {insufficient_security, no_suitable_public_key}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key)
    end.


do_negotiated(#{client_share := ClientKey,
                group := SelectedGroup,
                sign_alg := SignatureScheme
               } = Map,
           #state{connection_states = ConnectionStates0,
                  session = #session{session_id = SessionId,
                                     own_certificate = OwnCert},
                  ssl_options = #ssl_options{} = _SslOpts,
                  key_share = KeyShare,
                  handshake_env = #handshake_env{tls_handshake_history = _HHistory0},
                  connection_env = #connection_env{private_key = CertPrivateKey},
                  static_env = #static_env{
                                  cert_db = CertDbHandle,
                                  cert_db_ref = CertDbRef,
                                  socket = _Socket,
                                  transport_cb = _Transport}
                 } = State0) ->
    {Ref,Maybe} = maybe(),

    try
        %% Create server_hello
        %% Extensions: supported_versions, key_share, (pre_shared_key)
        ServerHello = server_hello(SessionId, KeyShare, ConnectionStates0, Map),

        {State1, _} = tls_connection:send_handshake(ServerHello, State0),

        State2 =
            calculate_handshake_secrets(ClientKey, SelectedGroup, KeyShare, State1),

        State3 = ssl_record:step_encryption_state(State2),

        %% Create EncryptedExtensions
        EncryptedExtensions = encrypted_extensions(),

        %% Encode EncryptedExtensions
        State4 = tls_connection:queue_handshake(EncryptedExtensions, State3),

        %% Create Certificate
        Certificate = certificate(OwnCert, CertDbHandle, CertDbRef, <<>>, server),

        %% Encode Certificate
        State5 = tls_connection:queue_handshake(Certificate, State4),

        %% Create CertificateVerify
        CertificateVerify = Maybe(certificate_verify(CertPrivateKey, SignatureScheme,
                                                     State5, server)),
        %% Encode CertificateVerify
        State6 = tls_connection:queue_handshake(CertificateVerify, State5),

        %% Create Finished
        Finished = finished(State6),

        %% Encode Finished
        State7 = tls_connection:queue_handshake(Finished, State6),

        %% Send first flight
        {State8, _} = tls_connection:send_handshake_flight(State7),

        State8

    catch
        {Ref, {state_not_implemented, State}} ->
            %% TODO
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {state_not_implemented, State})
    end.


do_wait_finished(#change_cipher_spec{},
              #state{connection_states = _ConnectionStates0,
                     session = #session{session_id = _SessionId,
                                        own_certificate = _OwnCert},
                     ssl_options = #ssl_options{} = _SslOpts,
                     key_share = _KeyShare,
                     handshake_env = #handshake_env{tls_handshake_history = _HHistory0},
                     static_env = #static_env{
                                     cert_db = _CertDbHandle,
                                     cert_db_ref = _CertDbRef,
                                     socket = _Socket,
                                     transport_cb = _Transport}
                    } = State0) ->
    %% {Ref,Maybe} = maybe(),

    try

        State0

    catch
        {_Ref, {state_not_implemented, State}} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {state_not_implemented, State})
    end;
do_wait_finished(#finished{},
              #state{connection_states = _ConnectionStates0,
                     session = #session{session_id = _SessionId,
                                        own_certificate = _OwnCert},
                     ssl_options = #ssl_options{} = _SslOpts,
                     key_share = _KeyShare,
                     handshake_env = #handshake_env{tls_handshake_history = _HHistory0},
                     static_env = #static_env{
                                     cert_db = _CertDbHandle,
                                     cert_db_ref = _CertDbRef,
                                     socket = _Socket,
                                     transport_cb = _Transport}
                    } = State0) ->

    %% {Ref,Maybe} = maybe(),

    try
        %% TODO: validate client Finished

        State1 = calculate_traffic_secrets(State0),

        %% Configure traffic keys
        ssl_record:step_encryption_state(State1)


    catch
        {_Ref, {state_not_implemented, State}} ->
            %% TODO
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {state_not_implemented, State})
    end.


%% TODO: Remove this function!
%% not_implemented(State) ->
%%     {error, {state_not_implemented, State}}.


calculate_handshake_secrets(ClientKey, SelectedGroup, KeyShare,
                              #state{connection_states = ConnectionStates,
                                     handshake_env =
                                         #handshake_env{
                                            tls_handshake_history = HHistory}} = State0) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo,
                         cipher_suite = CipherSuite} = SecParamsR,

    %% Calculate handshake_secret
    PSK = binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo)),
    EarlySecret = tls_v1:key_schedule(early_secret, HKDFAlgo , {psk, PSK}),
    PrivateKey = get_server_private_key(KeyShare),  %% #'ECPrivateKey'{}

    IKM = calculate_shared_secret(ClientKey, PrivateKey, SelectedGroup),
    HandshakeSecret = tls_v1:key_schedule(handshake_secret, HKDFAlgo, IKM, EarlySecret),

    %% Calculate [sender]_handshake_traffic_secret
    {Messages, _} =  HHistory,
    ClientHSTrafficSecret =
        tls_v1:client_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),
    ServerHSTrafficSecret =
        tls_v1:server_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),

    %% Calculate traffic keys
    #{cipher := Cipher} = ssl_cipher_format:suite_definition(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ClientHSTrafficSecret),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ServerHSTrafficSecret),

    %% Calculate Finished Keys
    ReadFinishedKey = tls_v1:finished_key(ClientHSTrafficSecret, HKDFAlgo),
    WriteFinishedKey = tls_v1:finished_key(ServerHSTrafficSecret, HKDFAlgo),

    update_pending_connection_states(State0, HandshakeSecret,
                                     ReadKey, ReadIV, ReadFinishedKey,
                                     WriteKey, WriteIV, WriteFinishedKey).

calculate_traffic_secrets(#state{connection_states = ConnectionStates,
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

    {Messages0, _} =  HHistory,

    %% Drop Client Finish
    [_|Messages] = Messages0,

    %% Calculate [sender]_application_traffic_secret_0
    ClientAppTrafficSecret0 =
        tls_v1:client_application_traffic_secret_0(HKDFAlgo, MasterSecret, lists:reverse(Messages)),
    ServerAppTrafficSecret0 =
        tls_v1:server_application_traffic_secret_0(HKDFAlgo, MasterSecret, lists:reverse(Messages)),

    %% Calculate traffic keys
    #{cipher := Cipher} = ssl_cipher_format:suite_definition(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ClientAppTrafficSecret0),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ServerAppTrafficSecret0),

    update_pending_connection_states(State0, MasterSecret,
                                     ReadKey, ReadIV, undefined,
                                     WriteKey, WriteIV, undefined).


get_server_private_key(#key_share_server_hello{server_share = ServerShare}) ->
    get_private_key(ServerShare).

get_private_key(#key_share_entry{
                   key_exchange = #'ECPrivateKey'{} = PrivateKey}) ->
    PrivateKey;
get_private_key(#key_share_entry{
                      key_exchange =
                          {_, PrivateKey}}) ->
    PrivateKey.

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


update_pending_connection_states(#state{connection_states =
                                            CS = #{pending_read := PendingRead0,
                                                   pending_write := PendingWrite0}} = State,
                                 HandshakeSecret,
                                 ReadKey, ReadIV, ReadFinishedKey,
                                 WriteKey, WriteIV, WriteFinishedKey) ->
    PendingRead = update_connection_state(PendingRead0, HandshakeSecret,
                                          ReadKey, ReadIV, ReadFinishedKey),
    PendingWrite = update_connection_state(PendingWrite0, HandshakeSecret,
                                           WriteKey, WriteIV, WriteFinishedKey),
    State#state{connection_states = CS#{pending_read => PendingRead,
                                        pending_write => PendingWrite}}.

update_connection_state(ConnectionState = #{security_parameters := SecurityParameters0},
                        HandshakeSecret, Key, IV, FinishedKey) ->
    %% Store secret
    SecurityParameters = SecurityParameters0#security_parameters{
                           master_secret = HandshakeSecret},
    ConnectionState#{security_parameters => SecurityParameters,
                     cipher_state => cipher_init(Key, IV, FinishedKey)}.



cipher_init(Key, IV, FinishedKey) ->
    #cipher_state{key = Key,
                  iv = IV,
                  finished_key = FinishedKey,
                  tag_len = 16}.


%% If there is no overlap between the received
%% "supported_groups" and the groups supported by the server, then the
%% server MUST abort the handshake with a "handshake_failure" or an
%% "insufficient_security" alert.
select_server_group(_, []) ->
    {error, {insufficient_security, no_suitable_groups}};
select_server_group(ServerGroups, [C|ClientGroups]) ->
    case lists:member(C, ServerGroups) of
        true ->
            {ok, C};
        false ->
            select_server_group(ServerGroups, ClientGroups)
    end.


%% RFC 8446 - 4.2.8.  Key Share
%% This vector MAY be empty if the client is requesting a
%% HelloRetryRequest.  Each KeyShareEntry value MUST correspond to a
%% group offered in the "supported_groups" extension and MUST appear in
%% the same order.  However, the values MAY be a non-contiguous subset
%% of the "supported_groups" extension and MAY omit the most preferred
%% groups.
%%
%% Clients can offer as many KeyShareEntry values as the number of
%% supported groups it is offering, each representing a single set of
%% key exchange parameters.
%%
%% Clients MUST NOT offer multiple KeyShareEntry values
%% for the same group.  Clients MUST NOT offer any KeyShareEntry values
%% for groups not listed in the client's "supported_groups" extension.
%% Servers MAY check for violations of these rules and abort the
%% handshake with an "illegal_parameter" alert if one is violated.
validate_key_share(_ ,[]) ->
    ok;
validate_key_share([], _) ->
    {error, illegal_parameter};
validate_key_share([G|ClientGroups], [{_, G, _}|ClientShares]) ->
    validate_key_share(ClientGroups, ClientShares);
validate_key_share([_|ClientGroups], [_|_] = ClientShares) ->
    validate_key_share(ClientGroups, ClientShares).


get_client_public_key(Group, ClientShares) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, {_, _, ClientPublicKey}} ->
             {ok, ClientPublicKey};
         false ->
             %% 4.1.4.  Hello Retry Request
             %%
             %% The server will send this message in response to a ClientHello
             %% message if it is able to find an acceptable set of parameters but the
             %% ClientHello does not contain sufficient information to proceed with
             %% the handshake.
             {error, {hello_retry_request, Group}}
     end.

select_cipher_suite([], _) ->
    {error, no_suitable_cipher};
select_cipher_suite([Cipher|ClientCiphers], ServerCiphers) ->
    case lists:member(Cipher, tls_v1:suites('TLS_v1.3')) andalso
        lists:member(Cipher, ServerCiphers) of
        true ->
            {ok, Cipher};
        false ->
            select_cipher_suite(ClientCiphers, ServerCiphers)
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
select_sign_algo(dsa, _ClientSignAlgs, _ServerSignAlgs) ->
    {error, {insufficient_security, no_suitable_public_key}};
%% TODO: Implement support for ECDSA keys!
select_sign_algo(_, [], _) ->
    {error, {insufficient_security, no_suitable_signature_algorithm}};
select_sign_algo(PublicKeyAlgo, [C|ClientSignAlgs], ServerSignAlgs) ->
    {_, S, _} = ssl_cipher:scheme_to_components(C),
    %% RSASSA-PKCS1-v1_5 and Legacy algorithms are not defined for use in signed
    %% TLS handshake messages: filter sha-1 and rsa_pkcs1.
    case ((PublicKeyAlgo =:= rsa andalso S =:= rsa_pss_rsae)
          orelse (PublicKeyAlgo =:= rsa_pss andalso S =:= rsa_pss_rsae))
        andalso
        lists:member(C, ServerSignAlgs) of
        true ->
            {ok, C};
        false ->
            select_sign_algo(PublicKeyAlgo, ClientSignAlgs, ServerSignAlgs)
    end.


do_check_cert_sign_algo(_, _, []) ->
    {error, {insufficient_security, no_suitable_signature_algorithm}};
do_check_cert_sign_algo(SignAlgo, SignHash, [Scheme|T]) ->
    {Hash, Sign, _Curve} = ssl_cipher:scheme_to_components(Scheme),
    case compare_sign_algos(SignAlgo, SignHash, Sign, Hash) of
        true ->
            ok;
        _Else ->
            do_check_cert_sign_algo(SignAlgo, SignHash, T)
    end.


%% id-RSASSA-PSS (rsa_pss) indicates that the key may only be used for PSS signatures.
%% TODO: Uncomment when rsa_pss signatures are supported in certificates
%% compare_sign_algos(rsa_pss, Hash, Algo, Hash)
%%   when Algo =:= rsa_pss_pss ->
%%     true;
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
    {SignAlgo0, _Param, PublicKeyAlgo0} = ssl_handshake:get_cert_params(Cert),
    {SignHash0, SignAlgo} = public_key:pkix_sign_types(SignAlgo0),
    %% Convert hash to new format
    SignHash = case SignHash0 of
                   sha ->
                       sha1;
                   H -> H
               end,
    PublicKeyAlgo = public_key_algo(PublicKeyAlgo0),
    {PublicKeyAlgo, SignAlgo, SignHash}.


%% Note: copied from ssl_handshake
public_key_algo(?'id-RSASSA-PSS') ->
    rsa_pss;
public_key_algo(?rsaEncryption) ->
    rsa;
public_key_algo(?'id-ecPublicKey') ->
    ecdsa;
public_key_algo(?'id-dsa') ->
    dsa.

get_signature_scheme_list(undefined) ->
    undefined;
get_signature_scheme_list(#signature_algorithms_cert{
                        signature_scheme_list = ClientSignatureSchemes}) ->
    ClientSignatureSchemes;
get_signature_scheme_list(#signature_algorithms{
                        signature_scheme_list = ClientSignatureSchemes}) ->
    ClientSignatureSchemes.

get_supported_groups(#supported_groups{supported_groups = Groups}) ->
    Groups.

get_key_shares(#key_share_client_hello{client_shares = ClientShares}) ->
    ClientShares.

maybe() ->
    Ref = erlang:make_ref(),
    Ok = fun(ok) -> ok;
            ({ok,R}) -> R;
            ({error,Reason}) ->
                 throw({Ref,Reason})
         end,
    {Ref,Ok}.
