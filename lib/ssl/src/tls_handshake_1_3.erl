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

%% Create handshake messages
-export([certificate/5,
         certificate_verify/4,
         encrypted_extensions/1]).

-export([do_start/2,
         do_negotiated/2,
         do_wait_cert/2,
         do_wait_cv/2,
         do_wait_finished/2,
         do_wait_sh/2,
         do_wait_ee/2,
         do_wait_cert_cr/2,
         get_ticket_data/3,
         maybe_add_binders/3,
         maybe_add_binders/4,
         maybe_automatic_session_resumption/1]).

-export([is_valid_binder/4]).

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
    #server_hello{server_version = {3,3}, %% legacy_version
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 0, %% legacy attribute
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
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
    Extensions = #{server_hello_selected_version => SupportedVersions},
    ssl_handshake:add_server_share(MsgType, Extensions, KeyShare);
server_hello_extensions(MsgType, KeyShare, undefined) ->
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
    Extensions = #{server_hello_selected_version => SupportedVersions},
    ssl_handshake:add_server_share(MsgType, Extensions, KeyShare);
server_hello_extensions(MsgType, KeyShare, {SelectedIdentity, _}) ->
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
    PreSharedKey = #pre_shared_key_server_hello{selected_identity = SelectedIdentity},
    Extensions = #{server_hello_selected_version => SupportedVersions,
                   pre_shared_key => PreSharedKey},
    ssl_handshake:add_server_share(MsgType, Extensions, KeyShare).


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


encrypted_extensions(#state{handshake_env = #handshake_env{alpn = undefined}}) ->
    #encrypted_extensions{
       extensions = #{}
      };
encrypted_extensions(#state{handshake_env = #handshake_env{alpn = ALPNProtocol}}) ->
    Extensions = ssl_handshake:add_alpn(#{}, ALPNProtocol),
    #encrypted_extensions{
       extensions = Extensions
      }.


certificate_request(SignAlgs0, SignAlgsCert0) ->
    %% Input arguments contain TLS 1.2 algorithms due to backward compatibility
    %% reasons. These {Hash, Algo} tuples must be filtered before creating the
    %% the extensions.
    SignAlgs = filter_tls13_algs(SignAlgs0),
    SignAlgsCert = filter_tls13_algs(SignAlgsCert0),
    Extensions0 = add_signature_algorithms(#{}, SignAlgs),
    Extensions = add_signature_algorithms_cert(Extensions0, SignAlgsCert),
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
    lists:filter(fun is_atom/1, Algo).


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
certificate(OwnCert, CertDbHandle, CertDbRef, _CRContext, Role) ->
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
            {error, {no_suitable_certificates, Error}};
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
    end.


certificate_verify(PrivateKey, SignatureScheme,
                   #state{connection_states = ConnectionStates,
                          handshake_env =
                              #handshake_env{
                                 tls_handshake_history = {Messages, _}}}, Role) ->
    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, write),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    {HashAlgo, _, _} =
        ssl_cipher:scheme_to_components(SignatureScheme),

    Context = lists:reverse(Messages),

    %% Transcript-Hash uses the HKDF hash function defined by the cipher suite.
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),
    ContextString = context_string(Role),

    %% Digital signatures use the hash function defined by the selected signature
    %% scheme.
    case sign(THash, ContextString, HashAlgo, PrivateKey) of
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
    NonceSize = byte_size(Nonce),
    BinExts = encode_extensions(Exts),
    {?NEW_SESSION_TICKET, <<?UINT32(LifeTime), ?UINT32(Age),
                            ?BYTE(NonceSize), Nonce/binary,
                            ?UINT16(TicketSize), Ticket/binary,
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


decode_handshake(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                                  ?BYTE(SID_length), Session_ID:SID_length/binary,
                                  Cipher_suite:2/binary, ?BYTE(Comp_method),
                                  ?UINT16(ExtLen), Extensions:ExtLen/binary>>)
  when Random =:= ?HELLO_RETRY_REQUEST_RANDOM ->
    HelloExtensions = ssl_handshake:decode_hello_extensions(Extensions, {3,4}, {Major, Minor},
                                                            hello_retry_request),
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       compression_method = Comp_method,
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
    #key_update{request_update = Update};
decode_handshake(Tag, HandshakeMsg) ->
    ssl_handshake:decode_handshake({3,4}, Tag, HandshakeMsg).

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
sign(THash, Context, HashAlgo, #'ECPrivateKey'{} = PrivateKey) ->
    Content = build_content(Context, THash),
    try public_key:sign(Content, HashAlgo, PrivateKey) of
        Signature ->
            {ok, Signature}
    catch
        error:badarg ->
            {error, badarg}
    end;
sign(THash, Context, HashAlgo, PrivateKey) ->
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


verify(THash, Context, HashAlgo, Signature, {?'id-ecPublicKey', PublicKey, PublicKeyParams}) ->
    Content = build_content(Context, THash),
    try public_key:verify(Content, HashAlgo, Signature, {PublicKey, PublicKeyParams}) of
        Result ->
            {ok, Result}
    catch
        error:badarg ->
            {error, badarg}
    end;
verify(THash, Context, HashAlgo, Signature, {?rsaEncryption, PublicKey, _PubKeyParams}) ->
    Content = build_content(Context, THash),

    %% The length of the Salt MUST be equal to the length of the output
    %% of the digest algorithm: rsa_pss_saltlen = -1
    try public_key:verify(Content, HashAlgo, Signature, PublicKey,
                    [{rsa_padding, rsa_pkcs1_pss_padding},
                     {rsa_pss_saltlen, -1},
                     {rsa_mgf1_md, HashAlgo}]) of
        Result ->
            {ok, Result}
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


%% TLS Server
do_start(#client_hello{cipher_suites = ClientCiphers,
                       session_id = SessionId,
                       extensions = Extensions} = _Hello,
         #state{connection_states = _ConnectionStates0,
                ssl_options = #{ciphers := ServerCiphers,
                                signature_algs := ServerSignAlgs,
                                supported_groups := ServerGroups0,
                                alpn_preferred_protocols := ALPNPreferredProtocols,
                                honor_cipher_order := HonorCipherOrder},
                session = #session{own_certificate = Cert}} = State0) ->
    ClientGroups0 = maps:get(elliptic_curves, Extensions, undefined),
    ClientGroups = get_supported_groups(ClientGroups0),
    ServerGroups = get_supported_groups(ServerGroups0),

    ClientShares0 = maps:get(key_share, Extensions, undefined),
    ClientShares = get_key_shares(ClientShares0),

    OfferedPSKs = get_offered_psks(Extensions),

    ClientALPN0 = maps:get(alpn, Extensions, undefined),
    ClientALPN = ssl_handshake:decode_alpn(ClientALPN0),

    ClientSignAlgs = get_signature_scheme_list(
                       maps:get(signature_algs, Extensions, undefined)),
    ClientSignAlgsCert = get_signature_scheme_list(
                           maps:get(signature_algs_cert, Extensions, undefined)),

    {Ref,Maybe} = maybe(),

    try
        %% Handle ALPN extension if ALPN is configured
        ALPNProtocol = Maybe(handle_alpn(ALPNPreferredProtocols, ClientALPN)),

        %% If the server does not select a PSK, then the server independently selects a
        %% cipher suite, an (EC)DHE group and key share for key establishment,
        %% and a signature algorithm/certificate pair to authenticate itself to
        %% the client.
        Cipher = Maybe(select_cipher_suite(HonorCipherOrder, ClientCiphers, ServerCiphers)),

        Groups = Maybe(select_common_groups(ServerGroups, ClientGroups)),
        Maybe(validate_client_key_share(ClientGroups, ClientShares)),

        {PublicKeyAlgo, SignAlgo, SignHash} = get_certificate_params(Cert),

        %% Check if client supports signature algorithm of server certificate
        Maybe(check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgs, ClientSignAlgsCert)),

        %% Select signature algorithm (used in CertificateVerify message).
        SelectedSignAlg = Maybe(select_sign_algo(PublicKeyAlgo, ClientSignAlgs, ServerSignAlgs)),

        %% Select client public key. If no public key found in ClientShares or
        %% ClientShares is empty, trigger HelloRetryRequest as we were able
        %% to find an acceptable set of parameters but the ClientHello does not
        %% contain sufficient information.
        {Group, ClientPubKey} = get_client_public_key(Groups, ClientShares),

        %% Generate server_share
        KeyShare = ssl_cipher:generate_server_share(Group),

        State1 = update_start_state(State0,
                                    #{cipher => Cipher,
                                      key_share => KeyShare,
                                      session_id => SessionId,
                                      group => Group,
                                      sign_alg => SelectedSignAlg,
                                      peer_public_key => ClientPubKey,
                                      alpn => ALPNProtocol}),

        %% 4.1.4.  Hello Retry Request
        %%
        %% The server will send this message in response to a ClientHello
        %% message if it is able to find an acceptable set of parameters but the
        %% ClientHello does not contain sufficient information to proceed with
        %% the handshake.
        case Maybe(send_hello_retry_request(State1, ClientPubKey, KeyShare, SessionId)) of
            {_, start} = NextStateTuple ->
                NextStateTuple;
            {_, negotiated} = NextStateTuple ->
                %% Exclude any incompatible PSKs.
                PSK = Maybe(handle_pre_shared_key(State1, OfferedPSKs, Cipher)),
                Maybe(session_resumption(NextStateTuple, PSK))
        end
    catch
        {Ref, {insufficient_security, no_suitable_groups}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_groups);
        {Ref, illegal_parameter} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
        {Ref, no_suitable_cipher} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher);
        {Ref, {insufficient_security, no_suitable_signature_algorithm}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, "No suitable signature algorithm");
        {Ref, {insufficient_security, no_suitable_public_key}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key);
        {Ref, no_application_protocol} ->
            ?ALERT_REC(?FATAL, ?NO_APPLICATION_PROTOCOL)
    end;
%% TLS Client
do_start(#server_hello{cipher_suite = SelectedCipherSuite,
                       session_id = SessionId,
                       extensions = Extensions},
         #state{static_env = #static_env{role = client,
                                         host = Host,
                                         port = Port,
                                         transport_cb = Transport,
                                         socket = Socket},
                handshake_env = #handshake_env{renegotiation = {Renegotiation, _}} = HsEnv,
                connection_env = #connection_env{negotiated_version = NegotiatedVersion},
                ssl_options = #{ciphers := ClientCiphers,
                                supported_groups := ClientGroups0,
                                use_ticket := UseTicket,
                                session_tickets := SessionTickets,
                                log_level := LogLevel} = SslOpts,
                session = #session{own_certificate = Cert} = Session0,
                connection_states = ConnectionStates0
               } = State0) ->
    ClientGroups = get_supported_groups(ClientGroups0),

    {Ref,Maybe} = maybe(),
    try
        ServerKeyShare = maps:get(key_share, Extensions, undefined),
        SelectedGroup = get_selected_group(ServerKeyShare),

        %% Upon receipt of this extension in a HelloRetryRequest, the client
        %% MUST verify that (1) the selected_group field corresponds to a group
        %% which was provided in the "supported_groups" extension in the
        %% original ClientHello and (2) the selected_group field does not
        %% correspond to a group which was provided in the "key_share" extension
        %% in the original ClientHello.  If either of these checks fails, then
        %% the client MUST abort the handshake with an "illegal_parameter"
        %% alert.
        Maybe(validate_selected_group(SelectedGroup, ClientGroups)),

        Maybe(validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),

        %% Otherwise, when sending the new ClientHello, the client MUST
        %% replace the original "key_share" extension with one containing only a
        %% new KeyShareEntry for the group indicated in the selected_group field
        %% of the triggering HelloRetryRequest.
        ClientKeyShare = ssl_cipher:generate_client_shares([SelectedGroup]),
        TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
        Hello0 = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
                                           SessionId, Renegotiation, Cert, ClientKeyShare,
                                           TicketData),

        %% Update state
        State1 = update_start_state(State0,
                                    #{cipher => SelectedCipherSuite,
                                      key_share => ClientKeyShare,
                                      session_id => SessionId,
                                      group => SelectedGroup}),

        %% Replace ClientHello1 with a special synthetic handshake message
        State2 = replace_ch1_with_message_hash(State1),
        #state{handshake_env = #handshake_env{tls_handshake_history = HHistory0}} = State2,

        %% Update pre_shared_key extension with binders (TLS 1.3)
        Hello = tls_handshake_1_3:maybe_add_binders(Hello0, HHistory0, TicketData, NegotiatedVersion),

        {BinMsg, ConnectionStates, HHistory} =
            tls_connection:encode_handshake(Hello,  NegotiatedVersion, ConnectionStates0, HHistory0),
        tls_socket:send(Transport, Socket, BinMsg),
        ssl_logger:debug(LogLevel, outbound, 'handshake', Hello),
        ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),

        State = State2#state{
                  connection_states = ConnectionStates,
                  session = Session0#session{session_id = Hello#client_hello.session_id},
                  handshake_env = HsEnv#handshake_env{tls_handshake_history = HHistory},
                  key_share = ClientKeyShare},

        {State, wait_sh}

    catch
        {Ref, {illegal_parameter, Reason}} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, Reason)
    end.


do_negotiated({start_handshake, PSK0},
              #state{connection_states = ConnectionStates0,
                     session = #session{session_id = SessionId,
                                        ecc = SelectedGroup,
                                        dh_public_value = ClientPublicKey},
                     ssl_options = #{} = SslOpts,
                     key_share = KeyShare} = State0) ->
    ServerPrivateKey = get_server_private_key(KeyShare),

    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{prf_algorithm = HKDF} = SecParamsR,


    {Ref,Maybe} = maybe(),
    try
        %% Create server_hello
        ServerHello = server_hello(server_hello, SessionId, KeyShare, PSK0, ConnectionStates0),

        {State1, _} = tls_connection:send_handshake(ServerHello, State0),

        PSK = get_pre_shared_key(PSK0, HKDF),

        State2 =
            calculate_handshake_secrets(ClientPublicKey, ServerPrivateKey, SelectedGroup,
                                        PSK, State1),

        State3 = ssl_record:step_encryption_state(State2),

        %% Create EncryptedExtensions
        EncryptedExtensions = encrypted_extensions(State2),

        %% Encode EncryptedExtensions
        State4 = tls_connection:queue_handshake(EncryptedExtensions, State3),

        %% Create and send CertificateRequest ({verify, verify_peer})
        {State5, NextState} = maybe_send_certificate_request(State4, SslOpts, PSK0),

        %% Create and send Certificate (if PSK is undefined)
        State6 = Maybe(maybe_send_certificate(State5, PSK0)),

        %% Create and send CertificateVerify (if PSK is undefined)
        State7 = Maybe(maybe_send_certificate_verify(State6, PSK0)),

        %% Create Finished
        Finished = finished(State7),

        %% Encode Finished
        State8 = tls_connection:queue_handshake(Finished, State7),

        %% Send first flight
        {State9, _} = tls_connection:send_handshake_flight(State8),

        {State9, NextState}

    catch
        {Ref, badarg} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {digitally_sign, badarg});
        {Ref, {no_suitable_certificates, Reason}} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {no_suitable_certificates, Reason})
    end.


do_wait_cert(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(process_certificate(Certificate, State0))
    catch
        {Ref, {certificate_required, State}} ->
            {?ALERT_REC(?FATAL, ?CERTIFICATE_REQUIRED, certificate_required), State};
        {Ref, {{certificate_unknown, Reason}, State}} ->
            {?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, Reason), State};
        {Ref, {{internal_error, Reason}, State}} ->
            {?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason), State};
        {Ref, {{handshake_failure, Reason}, State}} ->
            {?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason), State};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State};
        {#alert{} = Alert, State} ->
            {Alert, State}
    end.


do_wait_cv(#certificate_verify_1_3{} = CertificateVerify, State0) ->
    {Ref,Maybe} = maybe(),
    try
        State1 = Maybe(verify_signature_algorithm(State0, CertificateVerify)),
        Maybe(verify_certificate_verify(State1, CertificateVerify))
    catch
        {Ref, {{bad_certificate, Reason}, State}} ->
            {?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, {bad_certificate, Reason}), State};
        {Ref, {badarg, State}} ->
            {?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {verify, badarg}), State};
        {Ref, {{handshake_failure, Reason}, State}} ->
            {?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {handshake_failure, Reason}), State}
    end.

%% TLS Server
do_wait_finished(#finished{verify_data = VerifyData},
                 #state{static_env = #static_env{role = server}} = State0) ->
    {Ref,Maybe} = maybe(),

    try
        Maybe(validate_client_finished(State0, VerifyData)),

        State1 = calculate_traffic_secrets(State0),
        State2 = maybe_calculate_resumption_master_secret(State1),

        %% Configure traffic keys
        State3 = ssl_record:step_encryption_state(State2),

        %% Send session ticket
        maybe_send_session_ticket(State3, 3)

    catch
        {Ref, decrypt_error} ->
            ?ALERT_REC(?FATAL, ?DECRYPT_ERROR, decrypt_error)
    end;
%% TLS Client
do_wait_finished(#finished{verify_data = _VerifyData},
                 #state{static_env = #static_env{role = client}} = State0) ->

    {Ref,Maybe} = maybe(),

    try
        %% Maybe(validate_client_finished(State0, VerifyData)),

        %% Maybe send Certificate + CertificateVerify
        State1 = Maybe(maybe_queue_cert_cert_cv(State0)),

        Finished = finished(State1),

        %% Encode Finished
        State2 = tls_connection:queue_handshake(Finished, State1),

        %% Send first flight
        {State3, _} = tls_connection:send_handshake_flight(State2),

        State4 = calculate_traffic_secrets(State3),
        State5 = maybe_calculate_resumption_master_secret(State4),

        %% Configure traffic keys
        ssl_record:step_encryption_state(State5)

    catch
        {Ref, decrypt_error} ->
            ?ALERT_REC(?FATAL, ?DECRYPT_ERROR, decrypt_error);
        {Ref, badarg} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {digitally_sign, badarg});
        {Ref, {no_suitable_certificates, Reason}} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {no_suitable_certificates, Reason})
    end.


do_wait_sh(#server_hello{cipher_suite = SelectedCipherSuite,
                         session_id = SessionId,
                         extensions = Extensions} = ServerHello,
           #state{key_share = ClientKeyShare0,
                  ssl_options = #{ciphers := ClientCiphers,
                                  supported_groups := ClientGroups0,
                                  session_tickets := SessionTickets,
                                  use_ticket := UseTicket}} = State0) ->
    ClientGroups = get_supported_groups(ClientGroups0),
    ServerKeyShare0 = maps:get(key_share, Extensions, undefined),
    ServerPreSharedKey = maps:get(pre_shared_key, Extensions, undefined),
    SelectedIdentity = get_selected_identity(ServerPreSharedKey),
    ClientKeyShare = get_key_shares(ClientKeyShare0),

    {Ref,Maybe} = maybe(),
    try
        %% Go to state 'start' if server replies with 'HelloRetryRequest'.
        Maybe(maybe_hello_retry_request(ServerHello, State0)),

        %% Resumption and PSK
        State1 = handle_resumption(State0, SelectedIdentity),
        ServerKeyShare = get_key_shares(ServerKeyShare0),

        Maybe(validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),
        Maybe(validate_server_key_share(ClientGroups, ServerKeyShare)),

        %% Get server public key
        {SelectedGroup, ServerPublicKey} = get_server_public_key(ServerKeyShare),

        {_, ClientPrivateKey} = get_client_private_key([SelectedGroup], ClientKeyShare),

        %% Update state
        State2 = update_start_state(State1,
                                    #{cipher => SelectedCipherSuite,
                                     key_share => ClientKeyShare0,
                                     session_id => SessionId,
                                     group => SelectedGroup,
                                     peer_public_key => ServerPublicKey}),

        #state{connection_states = ConnectionStates} = State2,
        #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
        #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

        PSK = Maybe(get_pre_shared_key(SessionTickets, UseTicket, HKDFAlgo, SelectedIdentity)),
        State3 = calculate_handshake_secrets(ServerPublicKey, ClientPrivateKey, SelectedGroup,
                                             PSK, State2),
        State4 = ssl_record:step_encryption_state(State3),

        {State4, wait_ee}

    catch
        {Ref, {State, StateName, ServerHello}} ->
            {State, StateName, ServerHello};
        {Ref, {insufficient_security, no_suitable_groups}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_groups);
        {Ref, illegal_parameter} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
        {Ref, no_suitable_cipher} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher);
        {Ref, {insufficient_security, no_suitable_signature_algorithm}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, "No suitable signature algorithm");
        {Ref, {insufficient_security, no_suitable_public_key}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key)
    end.


do_wait_ee(#encrypted_extensions{extensions = Extensions}, State0) ->

    ALPNProtocol0 = maps:get(alpn, Extensions, undefined),
    ALPNProtocol = get_alpn(ALPNProtocol0),

    {Ref, Maybe} = maybe(),

    try
        %% Go to state 'wait_finished' if using PSK.
        Maybe(maybe_resumption(State0)),

        %% Update state
        #state{handshake_env = HsEnv} = State0,
        State1 = State0#state{handshake_env = HsEnv#handshake_env{alpn = ALPNProtocol}},

        {State1, wait_cert_cr}
    catch
        {Ref, {insufficient_security, no_suitable_groups}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_groups);
        {Ref, illegal_parameter} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
        {Ref, no_suitable_cipher} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher);
        {Ref, {insufficient_security, no_suitable_signature_algorithm}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, "No suitable signature algorithm");
        {Ref, {insufficient_security, no_suitable_public_key}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key);
        {Ref, {State, StateName}} ->
            {State, StateName}
    end.


do_wait_cert_cr(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(process_certificate(Certificate, State0))
    catch
        {Ref, {certificate_required, _State}} ->
            ?ALERT_REC(?FATAL, ?CERTIFICATE_REQUIRED, certificate_required);
        {Ref, {{certificate_unknown, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, Reason);
        {Ref, {{internal_error, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason);
        {Ref, {{handshake_failure, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end;
do_wait_cert_cr(#certificate_request_1_3{} = CertificateRequest, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(process_certificate_request(CertificateRequest, State0))
    catch
        {Ref, {certificate_required, _State}} ->
            ?ALERT_REC(?FATAL, ?CERTIFICATE_REQUIRED, certificate_required);
        {Ref, {{certificate_unknown, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, Reason);
        {Ref, {illegal_parameter, Reason}} ->
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, Reason);
        {Ref, {{internal_error, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason);
        {Ref, {{handshake_failure, Reason}, _State}} ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason)
    end.



%% TODO: Remove this function!
%% not_implemented(State, Reason) ->
%%     {error, {not_implemented, State, Reason}}.

%% not_implemented(update_secrets, State0, Reason) ->
%%     State1 = calculate_traffic_secrets(State0),
%%     State = ssl_record:step_encryption_state(State1),
%%     {error, {not_implemented, State, Reason}}.


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
maybe_hello_retry_request(#server_hello{random = ?HELLO_RETRY_REQUEST_RANDOM} = ServerHello, State0) ->
    {error, {State0, start, ServerHello}};
maybe_hello_retry_request(_, _) ->
    ok.


maybe_resumption(#state{handshake_env = #handshake_env{resumption = true}} = State) ->
    {error, {State, wait_finished}};
maybe_resumption(_) ->
    ok.


handle_resumption(State, undefined) ->
    State;
handle_resumption(#state{handshake_env = HSEnv0} = State, _) ->
    HSEnv = HSEnv0#handshake_env{resumption = true},
    State#state{handshake_env = HSEnv}.


maybe_queue_cert_cert_cv(#state{client_certificate_requested = false} = State) ->
    {ok, State};
maybe_queue_cert_cert_cv(#state{connection_states = _ConnectionStates0,
                                session = #session{session_id = _SessionId,
                                                   own_certificate = OwnCert},
                                ssl_options = #{} = _SslOpts,
                                key_share = _KeyShare,
                                handshake_env = #handshake_env{tls_handshake_history = _HHistory0},
                                static_env = #static_env{
                                                role = client,
                                                cert_db = CertDbHandle,
                                                cert_db_ref = CertDbRef,
                                                socket = _Socket,
                                                transport_cb = _Transport}
                               } = State0) ->
    {Ref,Maybe} = maybe(),
    try
        %% Create Certificate
        Certificate = Maybe(certificate(OwnCert, CertDbHandle, CertDbRef, <<>>, client)),

        %% Encode Certificate
        State1 = tls_connection:queue_handshake(Certificate, State0),

        %% Maybe create and queue CertificateVerify
        State = Maybe(maybe_queue_cert_verify(Certificate, State1)),
        {ok, State}
    catch
        {Ref, badarg} ->
            {error, badarg}
    end.


%% Clients MUST send this message whenever authenticating via a certificate
%% (i.e., when the Certificate message is non-empty).
maybe_queue_cert_verify(#certificate_1_3{certificate_list = []}, State) ->
    {ok, State};
maybe_queue_cert_verify(_Certificate,
                        #state{connection_states = _ConnectionStates0,
                               session = #session{sign_alg = SignatureScheme},
                               connection_env = #connection_env{private_key = CertPrivateKey},
                               static_env = #static_env{role = client}
                              } = State) ->
    {Ref,Maybe} = maybe(),
    try
        CertificateVerify = Maybe(certificate_verify(CertPrivateKey, SignatureScheme, State, client)),
        {ok, tls_connection:queue_handshake(CertificateVerify, State)}
    catch
        {Ref, badarg} ->
            {error, badarg}
    end.


%% Recipients of Finished messages MUST verify that the contents are
%% correct and if incorrect MUST terminate the connection with a
%% "decrypt_error" alert.
validate_client_finished(#state{connection_states = ConnectionStates,
                handshake_env =
                    #handshake_env{
                       tls_handshake_history = {Messages0, _}}}, VerifyData) ->
    #{security_parameters := SecParamsR,
     cipher_state := #cipher_state{finished_key = FinishedKey}} =
        ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

    %% Drop the client's finished message, it is not part of the handshake context
    %% when the client calculates its finished message.
    [_|Messages] = Messages0,

    ControlData = tls_v1:finished_verify_data(FinishedKey, HKDFAlgo, Messages),
    compare_verify_data(ControlData, VerifyData).


compare_verify_data(Data, Data) ->
    ok;
compare_verify_data(_, _) ->
    {error, decrypt_error}.


send_hello_retry_request(#state{connection_states = ConnectionStates0} = State0,
                         no_suitable_key, KeyShare, SessionId) ->
    ServerHello = server_hello(hello_retry_request, SessionId, KeyShare, undefined, ConnectionStates0),
    {State1, _} = tls_connection:send_handshake(ServerHello, State0),

    %% Update handshake history
    State2 = replace_ch1_with_message_hash(State1),

    {ok, {State2, start}};
send_hello_retry_request(State0, _, _, _) ->
    %% Suitable key found.
    {ok, {State0, negotiated}}.

session_resumption({#state{ssl_options = #{session_tickets := disabled}} = State, negotiated}, _) ->
    {ok, {State, negotiated}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets}} = State, negotiated}, undefined)
  when Tickets =/= disabled ->
    {ok, {State, negotiated}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets}} = State0, negotiated}, PSK)
  when Tickets =/= disabled ->
    State = handle_resumption(State0, ok),
    {ok, {State, negotiated, PSK}}.


%% Do not send CR during session resumption
maybe_send_certificate_request(State, _, PSK) when PSK =/= undefined ->
    {State, wait_finished};
maybe_send_certificate_request(State, #{verify := verify_none}, _) ->
    {State, wait_finished};
maybe_send_certificate_request(State, #{verify := verify_peer,
                                        signature_algs := SignAlgs,
                                        signature_algs_cert := SignAlgsCert}, _) ->
    CertificateRequest = certificate_request(SignAlgs, SignAlgsCert),
    {tls_connection:queue_handshake(CertificateRequest, State), wait_cert}.


maybe_send_certificate(State, PSK) when  PSK =/= undefined ->
    {ok, State};
maybe_send_certificate(#state{session = #session{own_certificate = OwnCert},
                              static_env = #static_env{
                                              cert_db = CertDbHandle,
                                              cert_db_ref = CertDbRef}} = State, _) ->
    case certificate(OwnCert, CertDbHandle, CertDbRef, <<>>, server) of
        {ok, Certificate} ->
            {ok, tls_connection:queue_handshake(Certificate, State)};
        Error ->
            Error
    end.


maybe_send_certificate_verify(State, PSK) when  PSK =/= undefined ->
    {ok, State};
maybe_send_certificate_verify(#state{session = #session{sign_alg = SignatureScheme},
                                     connection_env = #connection_env{
                                                         private_key = CertPrivateKey}} = State, _) ->
    case certificate_verify(CertPrivateKey, SignatureScheme, State, server) of
        {ok, CertificateVerify} ->
            {ok, tls_connection:queue_handshake(CertificateVerify, State)};
        Error ->
            Error
    end.


maybe_send_session_ticket(#state{ssl_options = #{session_tickets := disabled}} = State, _) ->
    %% Do nothing!
    State;
maybe_send_session_ticket(State, 0) ->
    State;
maybe_send_session_ticket(#state{connection_states = ConnectionStates,
                                 static_env = #static_env{trackers = Trackers}} = State0, N) ->
    Tracker = proplists:get_value(session_tickets_tracker, Trackers),
    #{security_parameters := SecParamsR} =
        ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDF,
                         resumption_master_secret = RMS} = SecParamsR, 
    Ticket = tls_server_session_ticket:new(Tracker, HKDF, RMS),
    {State, _} = tls_connection:send_handshake(Ticket, State0),
    maybe_send_session_ticket(State, N - 1).

process_certificate_request(#certificate_request_1_3{},
                            #state{session = #session{own_certificate = undefined}} = State) ->
    {ok, {State#state{client_certificate_requested = true}, wait_cert}};

process_certificate_request(#certificate_request_1_3{
                              extensions = Extensions},
                            #state{session = #session{own_certificate = Cert} = Session} = State) ->
    ServerSignAlgs = get_signature_scheme_list(
                       maps:get(signature_algs, Extensions, undefined)),
    ServerSignAlgsCert = get_signature_scheme_list(
                           maps:get(signature_algs_cert, Extensions, undefined)),

    {_PublicKeyAlgo, SignAlgo, SignHash} = get_certificate_params(Cert),

    %% Check if server supports signature algorithm of client certificate
    case check_cert_sign_algo(SignAlgo, SignHash, ServerSignAlgs, ServerSignAlgsCert) of
        ok ->
            {ok, {State#state{client_certificate_requested = true}, wait_cert}};
        {error, _} ->
            %% Certificate not supported: send empty certificate in state 'wait_finished'
            {ok, {State#state{client_certificate_requested = true,
                              session = Session#session{own_certificate = undefined}}, wait_cert}}
    end.


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
    {error, {certificate_required, State}};
process_certificate(#certificate_1_3{certificate_list = Certs0},
                    #state{ssl_options =
                               #{signature_algs := SignAlgs,
                                 signature_algs_cert := SignAlgsCert} = SslOptions,
                           static_env =
                               #static_env{
                                  role = Role,
                                  host = Host,
                                  cert_db = CertDbHandle,
                                  cert_db_ref = CertDbRef,
                                  crl_db = CRLDbHandle}} = State0) ->
    %% TODO: handle extensions!
    %% Remove extensions from list of certificates!
    Certs = convert_certificate_chain(Certs0),
    case is_supported_signature_algorithm(Certs, SignAlgs, SignAlgsCert) of
        true ->
            case validate_certificate_chain(Certs, CertDbHandle, CertDbRef,
                                            SslOptions, CRLDbHandle, Role, Host) of
                {ok, {PeerCert, PublicKeyInfo}} ->
                    State = store_peer_cert(State0, PeerCert, PublicKeyInfo),
                    {ok, {State, wait_cv}};
                {error, Reason} ->
                    State = update_encryption_state(Role, State0),
                    {error, {Reason, State}};
                {ok, #alert{} = Alert} ->
                    State = update_encryption_state(Role, State0),
                    {error, {Alert, State}}
            end;
        false ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {{handshake_failure,
                      "Client certificate uses unsupported signature algorithm"}, State}}
    end.


%% TODO: check whole chain!
is_supported_signature_algorithm(Certs, SignAlgs, undefined) ->
    is_supported_signature_algorithm(Certs, SignAlgs);
is_supported_signature_algorithm(Certs, _, SignAlgsCert) ->
    is_supported_signature_algorithm(Certs, SignAlgsCert).
%%
is_supported_signature_algorithm([BinCert|_], SignAlgs0) ->
    #'OTPCertificate'{signatureAlgorithm = SignAlg} =
        public_key:pkix_decode_cert(BinCert, otp),
    SignAlgs = filter_tls13_algs(SignAlgs0),
    Scheme = ssl_cipher:signature_algorithm_to_scheme(SignAlg),
    lists:member(Scheme, SignAlgs).


%% Sets correct encryption state when sending Alerts in shared states that use different secrets.
%% - If client: use handshake secrets.
%% - If server: use traffic secrets as by this time the client's state machine
%%              already stepped into the 'connection' state.
update_encryption_state(server, State0) ->
    State1 = calculate_traffic_secrets(State0),
    ssl_record:step_encryption_state(State1);
update_encryption_state(client, State) ->
    State.


validate_certificate_chain(Certs, CertDbHandle, CertDbRef,
                           #{server_name_indication := ServerNameIndication,
                             partial_chain := PartialChain,
                             verify_fun := VerifyFun,
                             customize_hostname_check := CustomizeHostnameCheck,
                             crl_check := CrlCheck,
                             log_level := LogLevel,
                             depth := Depth} = SslOptions,
                           CRLDbHandle, Role, Host) ->
    ServerName = ssl_handshake:server_name(ServerNameIndication, Host, Role),
    [PeerCert | ChainCerts ] = Certs,
    try
	{TrustedCert, CertPath}  =
	    ssl_certificate:trusted_cert_and_path(Certs, CertDbHandle, CertDbRef,
                                                  PartialChain),
        ValidationFunAndState =
            ssl_handshake:validation_fun_and_state(VerifyFun, Role,
                                     CertDbHandle, CertDbRef, ServerName,
                                     CustomizeHostnameCheck,
                                     CrlCheck, CRLDbHandle, CertPath, LogLevel),
        Options = [{max_path_length, Depth},
                   {verify_fun, ValidationFunAndState}],
        %% TODO: Validate if Certificate is using a supported signature algorithm
        %% (signature_algs_cert)!
        case public_key:pkix_path_validation(TrustedCert, CertPath, Options) of
            {ok, {PublicKeyInfo,_}} ->
                {ok, {PeerCert, PublicKeyInfo}};
            {error, Reason} ->
                {ok, ssl_handshake:handle_path_validation_error(Reason, PeerCert, ChainCerts,
                                                                SslOptions, Options,
                                                                CertDbHandle, CertDbRef)}
        end
    catch
        error:{badmatch,{error, {asn1, Asn1Reason}}} ->
            %% ASN-1 decode of certificate somehow failed
            {error, {certificate_unknown, {failed_to_decode_certificate, Asn1Reason}}};
        error:OtherReason ->
            {error, {internal_error, {unexpected_error, OtherReason}}}
    end.


store_peer_cert(#state{session = Session,
                       handshake_env = HsEnv} = State, PeerCert, PublicKeyInfo) ->
    State#state{session = Session#session{peer_certificate = PeerCert},
                handshake_env = HsEnv#handshake_env{public_key_info = PublicKeyInfo}}.


convert_certificate_chain(Certs) ->
    Fun = fun(#certificate_entry{data = Data}) ->
                  {true, Data};
             (_) ->
                  false
          end,
    lists:filtermap(Fun, Certs).


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
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ClientHSTrafficSecret),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ServerHSTrafficSecret),

    %% Calculate Finished Keys
    ReadFinishedKey = tls_v1:finished_key(ClientHSTrafficSecret, HKDFAlgo),
    WriteFinishedKey = tls_v1:finished_key(ServerHSTrafficSecret, HKDFAlgo),

    update_pending_connection_states(State0, HandshakeSecret, undefined,
                                     ReadKey, ReadIV, ReadFinishedKey,
                                     WriteKey, WriteIV, WriteFinishedKey).


%% Server
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
get_pre_shared_key(enabled = SessionTickets, UseTicket, HKDFAlgo, SelectedIdentity) ->
    TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
    case choose_psk(TicketData, SelectedIdentity) of
        undefined -> %% full handshake, default PSK
            {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
        illegal_parameter ->
            {error, illegal_parameter};
        {_, PSK} ->
            {ok, PSK}
    end;
get_pre_shared_key(auto = SessionTickets, UseTicket, HKDFAlgo, SelectedIdentity) ->
    TicketData = get_ticket_data(self(), SessionTickets, UseTicket),
    case choose_psk(TicketData, SelectedIdentity) of
        undefined -> %% full handshake, default PSK
            tls_client_ticket_store:unlock_tickets(self(), UseTicket),
            {ok, binary:copy(<<0>>, ssl_cipher:hash_size(HKDFAlgo))};
        illegal_parameter ->
            tls_client_ticket_store:unlock_tickets(self(), UseTicket),
            {error, illegal_parameter};
        {Key, PSK} ->
            tls_client_ticket_store:remove_tickets([Key]),  %% Remove single-use ticket
            tls_client_ticket_store:unlock_tickets(self(), UseTicket -- [Key]),
            {ok, PSK}
    end.


choose_psk(undefined, _) ->
    undefined;
choose_psk([], _) ->
    illegal_parameter;
choose_psk([{Key, SelectedIdentity, _, PSK, _, _}|_], SelectedIdentity) ->
    {Key, PSK};
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
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ClientAppTrafficSecret0),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ServerAppTrafficSecret0),

    update_pending_connection_states(State0, MasterSecret, undefined,
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


update_pending_connection_states(#state{
                                    static_env = #static_env{role = server},
                                    connection_states =
                                        CS = #{pending_read := PendingRead0,
                                               pending_write := PendingWrite0}} = State,
                                 HandshakeSecret, ResumptionMasterSecret,
                                 ReadKey, ReadIV, ReadFinishedKey,
                                 WriteKey, WriteIV, WriteFinishedKey) ->
    PendingRead = update_connection_state(PendingRead0, HandshakeSecret, ResumptionMasterSecret,
                                          ReadKey, ReadIV, ReadFinishedKey),
    PendingWrite = update_connection_state(PendingWrite0, HandshakeSecret, ResumptionMasterSecret,
                                           WriteKey, WriteIV, WriteFinishedKey),
    State#state{connection_states = CS#{pending_read => PendingRead,
                                        pending_write => PendingWrite}};
update_pending_connection_states(#state{
                                    static_env = #static_env{role = client},
                                    connection_states =
                                        CS = #{pending_read := PendingRead0,
                                               pending_write := PendingWrite0}} = State,
                                 HandshakeSecret, ResumptionMasterSecret,
                                 ReadKey, ReadIV, ReadFinishedKey,
                                 WriteKey, WriteIV, WriteFinishedKey) ->
    PendingRead = update_connection_state(PendingRead0, HandshakeSecret, ResumptionMasterSecret,
                                          WriteKey, WriteIV, WriteFinishedKey),
    PendingWrite = update_connection_state(PendingWrite0, HandshakeSecret, ResumptionMasterSecret,
                                           ReadKey, ReadIV, ReadFinishedKey),
    State#state{connection_states = CS#{pending_read => PendingRead,
                                        pending_write => PendingWrite}}.


update_connection_state(ConnectionState = #{security_parameters := SecurityParameters0},
                        HandshakeSecret, ResumptionMasterSecret, Key, IV, FinishedKey) ->
    %% Store secret
    SecurityParameters = SecurityParameters0#security_parameters{
                           master_secret = HandshakeSecret,
                           resumption_master_secret = ResumptionMasterSecret},
    ConnectionState#{security_parameters => SecurityParameters,
                     cipher_state => cipher_init(Key, IV, FinishedKey)}.


update_start_state(State, Map) ->
    Cipher = maps:get(cipher, Map, undefined),
    KeyShare = maps:get(key_share, Map, undefined),
    SessionId = maps:get(session_id, Map, undefined),
    Group = maps:get(group, Map, undefined),
    SelectedSignAlg = maps:get(sign_alg, Map, undefined),
    PeerPublicKey = maps:get(peer_public_key, Map, undefined),
    ALPNProtocol = maps:get(alpn, Map, undefined),
    update_start_state(State, Cipher, KeyShare, SessionId,
                       Group, SelectedSignAlg, PeerPublicKey,
                       ALPNProtocol).
%%
update_start_state(#state{connection_states = ConnectionStates0,
                          handshake_env = #handshake_env{} = HsEnv,
                          connection_env = CEnv,
                          session = Session} = State,
                   Cipher, KeyShare, SessionId,
                   Group, SelectedSignAlg, PeerPublicKey, ALPNProtocol) ->
    #{security_parameters := SecParamsR0} = PendingRead =
        maps:get(pending_read, ConnectionStates0),
    #{security_parameters := SecParamsW0} = PendingWrite =
        maps:get(pending_write, ConnectionStates0),
    SecParamsR = ssl_cipher:security_parameters_1_3(SecParamsR0, Cipher),
    SecParamsW = ssl_cipher:security_parameters_1_3(SecParamsW0, Cipher),
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
                connection_env = CEnv#connection_env{negotiated_version = {3,4}}}.


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


cipher_init(Key, IV, FinishedKey) ->
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
    case lists:member(PeerSignAlg, LocalSignAlgs) of
        true ->
            {ok, maybe_update_selected_sign_alg(State0, PeerSignAlg, Role)};
        false ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {{handshake_failure,
                      "CertificateVerify uses unsupported signature algorithm"}, State}}
    end.


maybe_update_selected_sign_alg(#state{session = Session} = State, SignAlg, client) ->
    State#state{session = Session#session{sign_alg = SignAlg}};
maybe_update_selected_sign_alg(State, _, _) ->
    State.


verify_certificate_verify(#state{
                             static_env = #static_env{role = Role},
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

    {HashAlgo, _, _} =
        ssl_cipher:scheme_to_components(SignatureScheme),

    Messages = get_handshake_context_cv(HHistory),

    Context = lists:reverse(Messages),

    %% Transcript-Hash uses the HKDF hash function defined by the cipher suite.
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),

    ContextString = peer_context_string(Role),

    %% Digital signatures use the hash function defined by the selected signature
    %% scheme.
    case verify(THash, ContextString, HashAlgo, Signature, PublicKeyInfo) of
        {ok, true} ->
            {ok, {State0, wait_finished}};
        {ok, false} ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {{handshake_failure, "Failed to verify CertificateVerify"}, State}};
        {error, badarg} ->
            State1 = calculate_traffic_secrets(State0),
            State = ssl_record:step_encryption_state(State1),
            {error, {badarg, State}}
    end.


context_string(server) ->
    <<"TLS 1.3, server CertificateVerify">>;
context_string(client) ->
    <<"TLS 1.3, client CertificateVerify">>.


%% Return context string for verifing peer signature
peer_context_string(server) ->
    <<"TLS 1.3, client CertificateVerify">>;
peer_context_string(client) ->
    <<"TLS 1.3, server CertificateVerify">>.


%% If there is no overlap between the received
%% "supported_groups" and the groups supported by the server, then the
%% server MUST abort the handshake with a "handshake_failure" or an
%% "insufficient_security" alert.
select_common_groups(_, []) ->
    {error, {insufficient_security, no_suitable_groups}};
select_common_groups(ServerGroups, ClientGroups) ->
    Fun = fun(E) -> lists:member(E, ClientGroups) end,
    case lists:filter(Fun, ServerGroups) of
        [] ->
            {error, {insufficient_security, no_suitable_groups}};
        L ->
            {ok, L}
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
validate_client_key_share(_ ,[]) ->
    ok;
validate_client_key_share([], _) ->
    {error, illegal_parameter};
validate_client_key_share([G|ClientGroups], [{_, G, _}|ClientShares]) ->
    validate_client_key_share(ClientGroups, ClientShares);
validate_client_key_share([_|ClientGroups], [_|_] = ClientShares) ->
    validate_client_key_share(ClientGroups, ClientShares).


%% Verify that selected group is offered by the client.
validate_server_key_share([G|_ClientGroups], {_, G, _}) ->
    ok;
validate_server_key_share([_|ClientGroups], {_, _, _} = ServerKeyShare) ->
    validate_server_key_share(ClientGroups, ServerKeyShare).


validate_selected_group(SelectedGroup, [SelectedGroup|_]) ->
    {error, {illegal_parameter,
             "Selected group sent by the server shall not correspond to a group"
             " which was provided in the key_share extension"}};
validate_selected_group(SelectedGroup, ClientGroups) ->
    case lists:member(SelectedGroup, ClientGroups) of
        true ->
            ok;
        false ->
            {error, {illegal_parameter,
                     "Selected group sent by the server shall correspond to a group"
                     " which was provided in the supported_groups extension"}}
    end.


get_client_public_key([Group|_] = Groups, ClientShares) ->
    get_client_public_key(Groups, ClientShares, Group).
%%
get_client_public_key(_, [], PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_public_key([], _, PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_public_key([Group|Groups], ClientShares, PreferredGroup) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, {_, _, ClientPublicKey}} ->
             {Group, ClientPublicKey};
         false ->
             get_client_public_key(Groups, ClientShares, PreferredGroup)
     end.

get_client_private_key([Group|_] = Groups, ClientShares) ->
    get_client_private_key(Groups, ClientShares, Group).
%%
get_client_private_key(_, [], PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_private_key([], _, PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_private_key([Group|Groups], ClientShares, PreferredGroup) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, {_, _, {_, ClientPrivateKey}}} ->
             {Group, ClientPrivateKey};
         {value, {_, _, #'ECPrivateKey'{} = ClientPrivateKey}} ->
             {Group, ClientPrivateKey};
         false ->
             get_client_private_key(Groups, ClientShares, PreferredGroup)
     end.


get_server_public_key({key_share_entry, Group, PublicKey}) ->
                             {Group, PublicKey}.


%% RFC 7301 - Application-Layer Protocol Negotiation Extension
%% It is expected that a server will have a list of protocols that it
%% supports, in preference order, and will only select a protocol if the
%% client supports it.  In that case, the server SHOULD select the most
%% highly preferred protocol that it supports and that is also
%% advertised by the client.  In the event that the server supports no
%% protocols that the client advertises, then the server SHALL respond
%% with a fatal "no_application_protocol" alert.
handle_alpn(undefined, _) ->
    {ok, undefined};
handle_alpn([], _) ->
    {error, no_application_protocol};
handle_alpn([_|_], undefined) ->
    {ok, undefined};
handle_alpn([ServerProtocol|T], ClientProtocols) ->
    case lists:member(ServerProtocol, ClientProtocols) of
        true ->
            {ok, ServerProtocol};
        false ->
            handle_alpn(T, ClientProtocols)
    end.


select_cipher_suite(_, [], _) ->
    {error, no_suitable_cipher};
%% If honor_cipher_order is set to true, use the server's preference for
%% cipher suite selection.
select_cipher_suite(true, ClientCiphers, ServerCiphers) ->
    select_cipher_suite(false, ServerCiphers, ClientCiphers);
select_cipher_suite(false, [Cipher|ClientCiphers], ServerCiphers) ->
    case lists:member(Cipher, tls_v1:suites('TLS_v1.3')) andalso
        lists:member(Cipher, ServerCiphers) of
        true ->
            {ok, Cipher};
        false ->
            select_cipher_suite(false, ClientCiphers, ServerCiphers)
    end.


%% RFC 8446 4.1.3 ServerHello
%% A client which receives a cipher suite that was not offered MUST abort the
%% handshake with an "illegal_parameter" alert.
validate_cipher_suite(Cipher, ClientCiphers) ->
    case lists:member(Cipher, ClientCiphers) of
        true ->
            ok;
        false ->
            {error, illegal_parameter}
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
select_sign_algo(_, [], _) ->
    {error, {insufficient_security, no_suitable_signature_algorithm}};
select_sign_algo(PublicKeyAlgo, [C|ClientSignAlgs], ServerSignAlgs) ->
    {_, S, _} = ssl_cipher:scheme_to_components(C),
    %% RSASSA-PKCS1-v1_5 and Legacy algorithms are not defined for use in signed
    %% TLS handshake messages: filter sha-1 and rsa_pkcs1.
    %%
    %% RSASSA-PSS RSAE algorithms: If the public key is carried in an X.509
    %% certificate, it MUST use the rsaEncryption OID.
    %% RSASSA-PSS PSS algorithms: If the public key is carried in an X.509 certificate,
    %% it MUST use the RSASSA-PSS OID.
    case ((PublicKeyAlgo =:= rsa andalso S =:= rsa_pss_rsae)
          orelse (PublicKeyAlgo =:= rsa_pss andalso S =:= rsa_pss_pss)
          orelse (PublicKeyAlgo =:= ecdsa andalso S =:= ecdsa))
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
    %% Filter unassigned and legacy elements
    lists:filter(fun (E) -> is_atom(E) andalso E =/= unassigned end,
                 ClientSignatureSchemes).

get_supported_groups(#supported_groups{supported_groups = Groups}) ->
    Groups.

get_key_shares(#key_share_client_hello{client_shares = ClientShares}) ->
    ClientShares;
get_key_shares(#key_share_server_hello{server_share = ServerShare}) ->
    ServerShare.

get_selected_identity(undefined) ->
    undefined;
get_selected_identity(#pre_shared_key_server_hello{selected_identity = SelectedIdentity}) ->
    SelectedIdentity.

get_offered_psks(Extensions) ->
    PSK = maps:get(pre_shared_key, Extensions, undefined),
    case PSK of
        undefined ->
            undefined;
        #pre_shared_key_client_hello{offered_psks = OfferedPSKs} ->
            OfferedPSKs
    end.


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
                      OfferedPreSharedKeys, Cipher) when Tickets =/= disabled ->
    Tracker = proplists:get_value(session_tickets_tracker, Trackers),
    #{prf := CipherHash} = ssl_cipher_format:suite_bin_to_map(Cipher),
    tls_server_session_ticket:use(Tracker, OfferedPreSharedKeys, CipherHash, HHistory).

get_selected_group(#key_share_hello_retry_request{selected_group = SelectedGroup}) ->
    SelectedGroup.

get_alpn(ALPNProtocol0) ->
    case ssl_handshake:decode_alpn(ALPNProtocol0) of
        undefined ->
            undefined;
        [ALPNProtocol] ->
            ALPNProtocol
    end.

maybe() ->
    Ref = erlang:make_ref(),
    Ok = fun(ok) -> ok;
            ({ok,R}) -> R;
            ({error,Reason}) ->
                 throw({Ref,Reason})
         end,
    {Ref,Ok}.


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
maybe_add_binders(Hello0, TicketData, Version) when Version =:= {3,4} ->
    HelloBin0 = tls_handshake:encode_handshake(Hello0, Version),
    HelloBin1 = iolist_to_binary(HelloBin0),
    Truncated = truncate_client_hello(HelloBin1),
    Binders = create_binders([Truncated], TicketData),
    update_binders(Hello0, Binders);
maybe_add_binders(Hello, _, Version) when Version =< {3,3} ->
    Hello.
%%
%% HelloRetryRequest
maybe_add_binders(Hello, _, undefined, _) ->
    Hello;
maybe_add_binders(Hello0, {[HRR,MessageHash|_], _}, TicketData, Version) when Version =:= {3,4} ->
    HelloBin0 = tls_handshake:encode_handshake(Hello0, Version),
    HelloBin1 = iolist_to_binary(HelloBin0),
    Truncated = truncate_client_hello(HelloBin1),
    Binders = create_binders([MessageHash,HRR,Truncated], TicketData),
    update_binders(Hello0, Binders);
maybe_add_binders(Hello, _, _, Version) when Version =< {3,3} ->
    Hello.


create_binders(Context, TicketData) ->
    create_binders(Context, TicketData, []).
%%
create_binders(_, [], Acc) ->
    lists:reverse(Acc);
create_binders(Context, [{_, _, _, PSK, _, HKDF}|T], Acc) ->
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
    HelloBin1 = remove_binders(HelloBin0),
    {Truncated, _} = split_binary(HelloBin1, size(HelloBin1) - 2),
    Truncated.


remove_binders(Binary0) ->
    OrigSize = byte_size(Binary0),
    HashSize256 = ssl_cipher:hash_size(sha256),
    HashSize384 = ssl_cipher:hash_size(sha384),
    HashSize512 = ssl_cipher:hash_size(sha512),

    NewSize256 = OrigSize - HashSize256 - 1,
    NewSize384 = OrigSize - HashSize384 - 1,
    NewSize512 = OrigSize - HashSize512 - 1,
    case Binary0 of
        <<Binary:NewSize256/binary,?BYTE(HashSize256),_:HashSize256/binary>> ->
            remove_binders(Binary);
        <<Binary:NewSize384/binary,?BYTE(HashSize384),_:HashSize384/binary>> ->
            remove_binders(Binary);
        <<Binary:NewSize512/binary,?BYTE(HashSize512),_:HashSize512/binary>> ->
            remove_binders(Binary);
        Else ->
            Else
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

%% Configure a suitable session ticket
maybe_automatic_session_resumption(#state{
                                      ssl_options = #{versions := [Version|_],
                                                      ciphers := UserSuites,
                                                      session_tickets := SessionTickets} = SslOpts0
                                     } = State0)
  when Version >= {3,4} andalso
       SessionTickets =:= auto ->
    AvailableCipherSuites = ssl_handshake:available_suites(UserSuites, Version),
    HashAlgos = cipher_hash_algos(AvailableCipherSuites),
    UseTicket = tls_client_ticket_store:find_ticket(self(), HashAlgos),
    tls_client_ticket_store:lock_tickets(self(), [UseTicket]),
    State = State0#state{ssl_options = SslOpts0#{use_ticket => [UseTicket]}},
    {[UseTicket], State};
maybe_automatic_session_resumption(#state{
                                      ssl_options = #{use_ticket := UseTicket}
                                     } = State) ->
    {UseTicket, State}.


cipher_hash_algos(Ciphers) ->
    Fun = fun(Cipher) ->
                  #{prf := Hash} = ssl_cipher_format:suite_bin_to_map(Cipher),
                  Hash
          end,
    lists:map(Fun, Ciphers).


get_ticket_data(_, undefined, _) ->
    undefined;
get_ticket_data(_, _, undefined) ->
    undefined;
get_ticket_data(_, enabled, UseTicket) ->
    process_user_tickets(UseTicket);
get_ticket_data(Pid, auto, UseTicket) ->
    tls_client_ticket_store:get_tickets(Pid, UseTicket).


process_user_tickets(UseTicket) ->
    process_user_tickets(UseTicket, [], 0).
%%
process_user_tickets([], Acc, _) ->
    lists:reverse(Acc);
process_user_tickets([H|T], Acc, N) ->
    #{hkdf := HKDF,
      sni := _SNI,
      psk := PSK,
      timestamp := Timestamp,
      ticket := NewSessionTicket} = erlang:binary_to_term(H),
    #new_session_ticket{
       ticket_lifetime = _LifeTime,
       ticket_age_add = AgeAdd,
       ticket_nonce = Nonce,
       ticket = Ticket,
       extensions = _Extensions
      } = NewSessionTicket,
    TicketAge =  erlang:system_time(seconds) - Timestamp,
    ObfuscatedTicketAge = obfuscate_ticket_age(TicketAge, AgeAdd),
    Identity = #psk_identity{
                  identity = Ticket,
                  obfuscated_ticket_age = ObfuscatedTicketAge},
    process_user_tickets(T, [{undefined, N, Identity, PSK, Nonce, HKDF}|Acc], N + 1).


%% The "obfuscated_ticket_age"
%% field of each PskIdentity contains an obfuscated version of the
%% ticket age formed by taking the age in milliseconds and adding the
%% "ticket_age_add" value that was included with the ticket
%% (see Section 4.6.1), modulo 2^32.
obfuscate_ticket_age(TicketAge, AgeAdd) ->
    (TicketAge + AgeAdd) rem round(math:pow(2,32)).
