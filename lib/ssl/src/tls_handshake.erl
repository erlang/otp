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
%% Purpose: Help functions for handling the TLS (specific parts of)
%%% SSL/TLS/DTLS handshake protocol
%%----------------------------------------------------------------------

-module(tls_handshake).
-moduledoc false.

-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").
-include("tls_record.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

%% Handshake handling
-export([client_hello/11, hello/5, hello/4]).

%% Handshake encoding
-export([encode_handshake/2]).

%% Handshake decoding
-export([get_tls_handshakes/4, decode_handshake/3]).

%% Handshake helper
-export([ocsp_nonce/1]).

-type tls_handshake() :: #client_hello{} | ssl_handshake:ssl_handshake().

%%====================================================================
%% Handshake handling
%%====================================================================
%%--------------------------------------------------------------------
-spec client_hello(ssl:host(), inet:port_number(), ssl_record:connection_states(),
		   ssl_options(), binary(), boolean(),
                   #key_share_client_hello{} | undefined, tuple() | undefined,
                   binary() | undefined, db_handle() | undefined, certdb_ref() | undefined) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(_Host, _Port, ConnectionStates,
	     #{versions := Versions,
               ciphers := UserSuites,
               fallback := Fallback
              } = SslOpts,
	     Id, Renegotiation, KeyShare, TicketData, OcspNonce, CertDbHandle, CertDbRef) ->
    Version = tls_record:highest_protocol_version(Versions),

    %% In TLS 1.3, the client indicates its version preferences in the
    %% "supported_versions" extension (Section 4.2.1) and the
    %% legacy_version field MUST be set to 0x0303, which is the version
    %% number for TLS 1.2.
    LegacyVersion =
        case tls_record:is_higher(Version, ?TLS_1_1) of
            true ->
                ?TLS_1_2;
            false ->
                Version
        end,
    #{security_parameters := SecParams} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    AvailableCipherSuites = ssl_handshake:available_suites(UserSuites, Version),
    Extensions = ssl_handshake:client_hello_extensions(Version,
						       AvailableCipherSuites,
						       SslOpts,
                                                       ConnectionStates,
                                                       Renegotiation,
                                                       KeyShare,
                                                       TicketData,
                                                       OcspNonce, CertDbHandle, CertDbRef),
    CipherSuites = ssl_handshake:cipher_suites(AvailableCipherSuites, Renegotiation, Fallback),
    #client_hello{session_id = Id,
		  client_version = LegacyVersion,
		  cipher_suites = CipherSuites,
		  random = SecParams#security_parameters.client_random,
		  extensions = Extensions
		 }.

%%--------------------------------------------------------------------
-spec hello(#server_hello{}, ssl_options(),
	    ssl_record:connection_states() | {inet:port_number(), #session{}, db_handle(),
				    atom(), ssl_record:connection_states(), 
				    binary() | undefined, ssl:kex_algo()},
	    boolean(), #session{}) ->
          {tls_record:tls_version(), ssl:session_id(), 
           ssl_record:connection_states(), alpn | npn, binary() | undefined, map()}|
          {atom(), atom(), tls_record:tls_version(), map()}.
                                                % Otherwise Throws #alert{}
%%
%% Description: Handles a received hello message
%%--------------------------------------------------------------------


%% TLS 1.3 - Section 4.1.3
%% TLS 1.3 clients receiving a ServerHello indicating TLS 1.2 or below
%% MUST check that the last eight bytes are not equal to either of these
%% values.
hello(#server_hello{server_version = {Major, Minor},
                    random = <<_:24/binary,Down:8/binary>>},
      #{versions := [{M,N}|_]}, _, _, _)
  when (M > 3 orelse M =:= 3 andalso N >= 4) andalso  %% TLS 1.3 client
       (Major =:= 3 andalso Minor =:= 3 andalso       %% Negotiating TLS 1.2
        Down =:= ?RANDOM_OVERRIDE_TLS12) orelse

       (M > 3 orelse M =:= 3 andalso N >= 4) andalso  %% TLS 1.3 client
       (Major =:= 3 andalso Minor < 3 andalso         %% Negotiating TLS 1.1 or prior
        Down =:= ?RANDOM_OVERRIDE_TLS11) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));

%% TLS 1.2 clients SHOULD also check that the last eight bytes are not
%% equal to the second value if the ServerHello indicates TLS 1.1 or below.
hello(#server_hello{server_version = {Major, Minor},
                    random = <<_:24/binary,Down:8/binary>>},
      #{versions := [{M,N}|_]}, _, _, _)
  when (M =:= 3 andalso N =:= 3) andalso              %% TLS 1.2 client
       (Major =:= 3 andalso Minor < 3 andalso         %% Negotiating TLS 1.1 or prior
        Down =:= ?RANDOM_OVERRIDE_TLS11) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));


%% TLS 1.3 - 4.2.1.  Supported Versions
%% If the "supported_versions" extension in the ServerHello contains a
%% version not offered by the client or contains a version prior to TLS
%% 1.3, the client MUST abort the handshake with an "illegal_parameter"
%% alert.
%%--------------------------------------------------------------------
%% TLS 1.2 Client
%%
%% - If "supported_version" is present (ServerHello):
%%   - Abort handshake with an "illegal_parameter" alert
hello(#server_hello{server_version = LegacyVersion,
                    random = Random,
		    cipher_suite = CipherSuite,
		    session_id = SessionId,
                    extensions =
                        #{server_hello_selected_version :=
                              #server_hello_selected_version{
                                 selected_version = Version}} = HelloExt},
      #{versions := SupportedVersions} = SslOpt,
      ConnectionStates0, Renegotiation, OldId) ->
    %% In TLS 1.3, the TLS server indicates its version using the "supported_versions" extension
    %% (Section 4.2.1), and the legacy_version field MUST be set to 0x0303, which is the version
    %% number for TLS 1.2.
    %% The "supported_versions" extension is supported from TLS 1.2.
    case ?TLS_LT(LegacyVersion, ?TLS_1_2) orelse
        LegacyVersion =:= ?TLS_1_2 andalso ?TLS_LT(Version,  ?TLS_1_2) of
        true ->
            throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
        false ->
            case tls_record:is_acceptable_version(Version, SupportedVersions) of
                true ->
                    case Version of
                        ?TLS_1_2 ->
                            IsNew = ssl_session:is_new(OldId, SessionId),
                            %% TLS 1.2 ServerHello with "supported_versions" (special case)
                            handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
                                                           HelloExt, SslOpt,
                                                           ConnectionStates0, Renegotiation, IsNew);
                        SelectedVersion ->
                            %% TLS 1.3 status_request and OCSP
                            %% responses provided in Certificate
                            %% messages
                            {next_state, wait_sh, SelectedVersion,
                             #{configured => maps:is_key(stapling, SslOpt),
                               status => not_negotiated}}
                    end;
                false ->
                    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
            end
    end;

hello(#server_hello{server_version = Version,
                    random = Random,
		    cipher_suite = CipherSuite,
		    session_id = SessionId,
                    extensions = HelloExt},
      #{versions := SupportedVersions} = SslOpt,
      ConnectionStates0, Renegotiation, OldId) ->
    IsNew = ssl_session:is_new(OldId, SessionId),
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
	true ->
	    handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
					   HelloExt, SslOpt,
                                           ConnectionStates0, Renegotiation, IsNew);
	false ->
	    throw(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION))
    end.


%%--------------------------------------------------------------------
-spec hello(#client_hello{}, ssl_options(),
	    {pid(), #session{}, ssl_record:connection_states(),
             list(), ssl:kex_algo()},
	    boolean()) ->
		   {tls_record:tls_version(), ssl:session_id(), 
		    ssl_record:connection_states(), alpn | npn, binary() | undefined}|
		   {tls_record:tls_version(), {resumed | new, #session{}}, 
		    ssl_record:connection_states(), binary() | undefined, 
                    HelloExt::map(), {ssl:hash(), ssl:sign_algo()} | 
                    undefined} | {atom(), atom()} | {atom(), atom(), tuple()}.
%% TLS 1.2 Server
%% - If "supported_versions" is present (ClientHello):
%%   - Select version from "supported_versions" (ignore ClientHello.legacy_version)
%%   - If server only supports versions greater than "supported_versions":
%%     - Abort handshake with a "protocol_version" alert (*)
%% - If "supported_versions" is absent (ClientHello):
%%   - Negotiate the minimum of ClientHello.legacy_version and TLS 1.2 (**)
%%   - If server only supports versions greater than ClientHello.legacy_version:
%%     - Abort handshake with a "protocol_version" alert
%%
%% (*)  Sends alert even if there is a gap in supported versions
%%      e.g. Server 1.0,1.2 Client 1.1,1.3
%% (**) Current implementation can negotiate a version not supported by the client
%%      e.g. Server 1.0,1.2 Client 1.1 -> ServerHello 1.0
hello(#client_hello{client_version = _ClientVersion,
		    cipher_suites = CipherSuites,
                    extensions = #{client_hello_versions :=
                                       #client_hello_versions{versions = ClientVersions}
                                  }} = Hello,
      #{versions := Versions} = SslOpts,
      Info, Renegotiation) ->
    try
        Version = ssl_handshake:select_supported_version(ClientVersions, Versions),
        do_hello(Version, Versions, CipherSuites, Hello, SslOpts, Info, Renegotiation)
    catch
	error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{reason,Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, malformed_handshake_data))
    end;

hello(#client_hello{client_version = ClientVersion,
		    cipher_suites = CipherSuites} = Hello,
      #{versions := Versions} = SslOpts,
      Info, Renegotiation) ->
    try
	Version = ssl_handshake:select_version(tls_record, ClientVersion, Versions),
        do_hello(Version, Versions, CipherSuites, Hello, SslOpts, Info, Renegotiation)
    catch
        error:{case_clause,{asn1, Asn1Reason}} ->
            %% ASN-1 decode of certificate somehow failed
            throw(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {failed_to_decode_own_certificate, Asn1Reason}));
        error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{reason,Reason}, {stacktrace, ST}]),
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, malformed_handshake_data))
    end.

%%--------------------------------------------------------------------
%%% Handshake encodeing
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec encode_handshake(tls_handshake() | tls_handshake_1_3:tls_handshake_1_3(),
                       tls_record:tls_version()) -> iolist().
%%     
%% Description: Encode a handshake packet
%%--------------------------------------------------------------------
encode_handshake(Package, Version) ->
    {MsgType, Bin} = enc_handshake(Package, Version),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), Bin].


%%--------------------------------------------------------------------
%%% Handshake decoding
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec get_tls_handshakes(tls_record:tls_version(), binary(), binary() | iolist(),
                        ssl_options()) ->
     {[{tls_handshake(), binary()}], binary()}.
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of handshake messages, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshakes(Version, Data, <<>>, Options) ->
    get_tls_handshakes_aux(Version, Data, Options, []);
get_tls_handshakes(Version, Data, Buffer, Options) ->
    get_tls_handshakes_aux(Version, list_to_binary([Buffer, Data]), Options, []).

%%--------------------------------------------------------------------
%%% Handshake helper
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec ocsp_nonce(map()) -> binary() | undefined.
%%
%% Description: Get an OCSP nonce
%%--------------------------------------------------------------------
ocsp_nonce(SslOpts) ->
    case maps:get(stapling, SslOpts, disabled) of
        #{ocsp_nonce := true} ->
            public_key:der_encode('Nonce', crypto:strong_rand_bytes(8));
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(Version, 
                    #client_hello{session_id = SugesstedId,
                                  cipher_suites = CipherSuites,
                                  random = Random,
                                  extensions = HelloExt},
		    #{versions := Versions,
                      eccs := SupportedECCs,
                      honor_ecc_order := ECCOrder} = SslOpts,
		    {SessIdTracker, Session0, ConnectionStates0, CertKeyPairs, _},
                    Renegotiation) ->
    case tls_record:is_acceptable_version(Version, Versions) of
	true ->
            SupportedHashSigns = supported_hashsigns(maps:get(signature_algs, SslOpts, undefined)),
            Curves = maps:get(elliptic_curves, HelloExt, undefined),
            ClientHashSigns = get_signature_ext(signature_algs, HelloExt, Version),
            ClientSignatureSchemes = get_signature_ext(signature_algs_cert, HelloExt, Version),
	    AvailableHashSigns = ssl_handshake:available_signature_algs(
				   ClientHashSigns, SupportedHashSigns, Version),
	    ECCCurve = ssl_handshake:select_curve(Curves, SupportedECCs, ECCOrder),
	    {Type, #session{cipher_suite = CipherSuite,
                            own_certificates = [OwnCert |_]} = Session1}
		= ssl_handshake:select_session(SugesstedId, CipherSuites,
                                               AvailableHashSigns,
					       SessIdTracker, Session0#session{ecc = ECCCurve},
                                               Version, SslOpts, CertKeyPairs),
	    case CipherSuite of
		no_suite ->
                    throw(?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_ciphers));
		_ ->
		    #{key_exchange := KeyExAlg} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
                    case ssl_handshake:select_hashsign({ClientHashSigns, ClientSignatureSchemes},
                                                       OwnCert, KeyExAlg,
                                                       SupportedHashSigns,
                                                       Version) of
			#alert{} = Alert ->
			    throw(Alert);
			HashSign ->
			    handle_client_hello_extensions(Version, Type, Random, 
                                                           CipherSuites, HelloExt,
							   SslOpts, Session1, 
                                                           ConnectionStates0,
                                                           Renegotiation, HashSign)
		    end
	    end;
	false ->
	    throw(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION))
    end.

supported_hashsigns(undefined) ->
    undefined;
supported_hashsigns(SigAlgs) ->
    ssl_cipher:signature_schemes_1_2(SigAlgs).

handle_client_hello_extensions(Version, Type, Random, CipherSuites,
                               HelloExt, SslOpts, Session0, ConnectionStates0, 
                               Renegotiation, HashSign) ->
    {Session, ConnectionStates, Protocol, ServerHelloExt} =
        ssl_handshake:handle_client_hello_extensions(tls_record, Random, CipherSuites,
                                                     HelloExt, Version, SslOpts,
                                                     Session0, ConnectionStates0,
                                                     Renegotiation,
                                                     Session0#session.is_resumable),
    {Version, {Type, Session}, ConnectionStates, Protocol, ServerHelloExt, HashSign}.

handle_server_hello_extensions(Version, SessionId, Random, CipherSuite,
                               HelloExt, SslOpt, ConnectionStates0, Renegotiation, IsNew) ->
    {ConnectionStates, ProtoExt, Protocol, StaplingState} =
        ssl_handshake:handle_server_hello_extensions(tls_record, Random, CipherSuite,
                                                     HelloExt, Version,
                                                     SslOpt, ConnectionStates0,
                                                     Renegotiation, IsNew),
    {Version, SessionId, ConnectionStates, ProtoExt, Protocol, StaplingState}.

do_hello(undefined, _Versions, _CipherSuites, _Hello, _SslOpts, _Info, _Renegotiation) ->
    throw(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION));
do_hello(Version, Versions, CipherSuites, Hello, SslOpts, Info, Renegotiation) ->
    case ssl_cipher:is_fallback(CipherSuites) of
        true ->
            Highest = tls_record:highest_protocol_version(Versions),
            case tls_record:is_higher(Highest, Version) of
                true ->
                    throw(?ALERT_REC(?FATAL, ?INAPPROPRIATE_FALLBACK));
                false ->
                    handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation)
            end;
        false ->
            handle_client_hello(Version, Hello, SslOpts, Info, Renegotiation)
    end.

%%--------------------------------------------------------------------
enc_handshake(#hello_request{}, Version) when ?TLS_LT(Version, ?TLS_1_3)->
    {?HELLO_REQUEST, <<>>};
enc_handshake(#client_hello{client_version = ServerVersion,
		     random = Random,
		     session_id = SessionID,
		     cipher_suites = CipherSuites,
		     extensions = HelloExtensions}, _Version) ->
    SIDLength = byte_size(SessionID),
    BinCompMethods = list_to_binary([?NO_COMPRESSION]),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    ExtensionsBin = ssl_handshake:encode_hello_extensions(HelloExtensions),
    {Major,Minor} = ServerVersion,

    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		      ?BYTE(SIDLength), SessionID/binary,
		      ?UINT16(CsLength), BinCipherSuites/binary,
		      ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};
enc_handshake(HandshakeMsg, ?TLS_1_3) ->
    tls_handshake_1_3:encode_handshake(HandshakeMsg);
enc_handshake(HandshakeMsg, Version) ->
    ssl_handshake:encode_handshake(HandshakeMsg, Version).

%%--------------------------------------------------------------------
get_tls_handshakes_aux(Version, <<?BYTE(Type), ?UINT24(Length),
                                  Body:Length/binary,Rest/binary>>,
                      #{log_level := LogLevel} = Opts,  Acc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    try decode_handshake(Version, Type, Body) of
        Handshake ->
            ssl_logger:debug(LogLevel, inbound, 'handshake', Handshake),
            get_tls_handshakes_aux(Version, Rest, Opts, [{Handshake,Raw} | Acc])
    catch
        error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{reason,Reason}, {stacktrace, ST}]),
            throw(?ALERT_REC(?FATAL, ?DECODE_ERROR, handshake_decode_error))
    end;
get_tls_handshakes_aux(_Version, Data, _, Acc) ->
    {lists:reverse(Acc), Data}.

decode_handshake(Version, ?HELLO_REQUEST, <<>>)
  when ?TLS_LT(Version, ?TLS_1_3) ->
    #hello_request{};
decode_handshake(Version, ?CLIENT_HELLO,
                 <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                   ?BYTE(SID_length), Session_ID:SID_length/binary,
                   ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
                   ?BYTE(Cm_length), _CompMethods:Cm_length/binary,
                   Extensions/binary>>) ->
    Exts = ssl_handshake:decode_vector(Extensions),
    DecodedExtensions = ssl_handshake:decode_hello_extensions(Exts, Version, {Major, Minor},
                                                              client_hello),
    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suites = ssl_handshake:decode_suites('2_bytes', CipherSuites),
       extensions = DecodedExtensions
      };
decode_handshake(?TLS_1_3, Tag, Msg) ->
    tls_handshake_1_3:decode_handshake(Tag, Msg);
decode_handshake(Version, Tag, Msg) ->
    ssl_handshake:decode_handshake(Version, Tag, Msg).

get_signature_ext(Ext, HelloExt, ?TLS_1_2) ->
    case maps:get(Ext, HelloExt, undefined) of
        %% Signature algorithms was not sent
        undefined ->
            undefined;
        %% Can happen when connection is upgraded and sni_fun changes
        %% the versions option from default
        #signature_algorithms{signature_scheme_list = Schemes} ->
            #hash_sign_algos{hash_sign_algos = ssl_cipher:signature_schemes_1_2(Schemes)};
        #signature_algorithms_cert{} = Algos ->
            Algos;
        #hash_sign_algos{} = Algos ->
            Algos
    end;
get_signature_ext(Ext, HelloExt, _) ->
    maps:get(Ext, HelloExt, undefined).


