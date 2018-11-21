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
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Encode
-export([encode_handshake/1, decode_handshake/2]).

%% Handshake
-export([handle_client_hello/3]).

%% Create handshake messages
-export([server_hello/4]).

%%====================================================================
%% Create handshake messages
%%====================================================================

server_hello(SessionId, KeyShare, ConnectionStates, _Map) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    Extensions = server_hello_extensions(KeyShare),
    #server_hello{server_version = {3,3}, %% legacy_version
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method =
                      SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  extensions = Extensions
		 }.

server_hello_extensions(KeyShare) ->
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
    Extensions = #{server_hello_selected_version => SupportedVersions},
    ssl_handshake:add_server_share(Extensions, KeyShare).



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
                    entries = Entries}) ->
    EncContext = encode_cert_req_context(Context),
    EncEntries = encode_cert_entries(Entries),
    {?CERTIFICATE, <<EncContext/binary, EncEntries/binary>>};
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
       entries = CertList
      };
decode_handshake(?CERTIFICATE, <<?BYTE(CSize), Context:CSize/binary,
                                 ?UINT24(Size), Certs:Size/binary>>) ->
    CertList = decode_cert_entries(Certs),
    #certificate_1_3{ 
       certificate_request_context = Context,
       entries = CertList
      };
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
    ExtSize = byte_size(BinExts),
    encode_cert_entries(Rest, 
                        [<<?UINT24(DSize), Data/binary, ?UINT16(ExtSize), BinExts/binary>> | Acc]).

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


%%====================================================================
%% Handle handshake messages
%%====================================================================

handle_client_hello(#client_hello{cipher_suites = ClientCiphers,
                                  random = Random,
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
        _ClientPubKey = Maybe(get_client_public_key(Group, ClientShares)),

        %% Handle certificate
        {PublicKeyAlgo, SignAlgo} = get_certificate_params(Cert),

        %% Check if client supports signature algorithm of server certificate
        Maybe(check_cert_sign_algo(SignAlgo, ClientSignAlgs, ClientSignAlgsCert)),

        %% Check if server supports
        SelectedSignAlg = Maybe(select_sign_algo(PublicKeyAlgo, ClientSignAlgs, ServerSignAlgs)),

        %% Generate server_share
        KeyShare = ssl_cipher:generate_server_share(Group),

        _Ret = #{cipher => Cipher,
                group => Group,
                sign_alg => SelectedSignAlg,
                %% client_share => ClientPubKey,
                key_share => KeyShare,
                client_random => Random,
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
        {Ref, {client_hello_retry_request, _Group0}} ->
            %% TODO
            exit({client_hello_retry_request, not_implemented});
        {Ref, no_suitable_cipher} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher);
        {Ref, {insufficient_security, no_suitable_signature_algorithm}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
        {Ref, {insufficient_security, no_suitable_public_key}} ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_public_key)
    end.


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
             %% ClientHelloRetryRequest
             {error, {client_hello_retry_request, Group}}
     end.

select_cipher_suite([], _) ->
    {error, no_suitable_cipher};
select_cipher_suite([Cipher|ClientCiphers], ServerCiphers) ->
    case lists:member(Cipher, ServerCiphers) of
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
check_cert_sign_algo(SignAlgo, ClientSignAlgs, undefined) ->
    maybe_lists_member(SignAlgo, ClientSignAlgs,
                       {insufficient_security, no_suitable_signature_algorithm});
check_cert_sign_algo(SignAlgo, _, ClientSignAlgsCert) ->
    maybe_lists_member(SignAlgo, ClientSignAlgsCert,
                       {insufficient_security, no_suitable_signature_algorithm}).


%% DSA keys are not supported by TLS 1.3
select_sign_algo(dsa, _ClientSignAlgs, _ServerSignAlgs) ->
    {error, {insufficient_security, no_suitable_public_key}};
%% TODO: Implement check for ellipctic curves!
select_sign_algo(PublicKeyAlgo, [C|ClientSignAlgs], ServerSignAlgs) ->
    {_, S, _} = ssl_cipher:scheme_to_components(C),
    case PublicKeyAlgo =:= rsa andalso
        ((S =:= rsa_pkcs1) orelse (S =:= rsa_pss_rsae) orelse (S =:= rsa_pss_pss)) andalso
        lists:member(C, ServerSignAlgs) of
        true ->
            {ok, C};
        false ->
            select_sign_algo(PublicKeyAlgo, ClientSignAlgs, ServerSignAlgs)
    end.


maybe_lists_member(Elem, List, Error) ->
    case lists:member(Elem, List) of
        true ->
            ok;
        false ->
            {error, Error}
    end.

%% TODO: test with ecdsa, rsa_pss_rsae, rsa_pss_pss
get_certificate_params(Cert) ->
    {SignAlgo0, _Param, PublicKeyAlgo0} = ssl_handshake:get_cert_params(Cert),
    SignAlgo = public_key:pkix_sign_types(SignAlgo0),
    PublicKeyAlgo = public_key_algo(PublicKeyAlgo0),
    Scheme = sign_algo_to_scheme(SignAlgo),
    {PublicKeyAlgo, Scheme}.

sign_algo_to_scheme({Hash0, Sign0}) ->
    SupportedSchemes = tls_v1:default_signature_schemes({3,4}),
    Hash = case Hash0 of
               sha ->
                   sha1;
               H ->
                   H
           end,
    Sign = case Sign0 of
               rsa ->
                   rsa_pkcs1;
               S ->
                   S
           end,
    sign_algo_to_scheme(Hash, Sign, SupportedSchemes).
%%
sign_algo_to_scheme(_, _, []) ->
    not_found;
sign_algo_to_scheme(H, S, [Scheme|T]) ->
    {Hash, Sign, _Curve} = ssl_cipher:scheme_to_components(Scheme),
    case H =:= Hash andalso S =:= Sign of
        true ->
            Scheme;
        false ->
            sign_algo_to_scheme(H, S, T)
    end.


%% Note: copied from ssl_handshake
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
