%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024 All Rights Reserved.
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
%% Purpose: Help functions for handling certificate verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%% The basic verification checks are done by
%% public_key:pkix_path_validation/3
%%
%% TLS code handles construction of alternative certificate paths
%% that can be used as input to public_key:pkix_path_validation/3
%% to try and find one path that is considered valid.
%%
%% The TLS protocol will send certificate chains that should consist
%% of [PeerCert, CA0 ... CAN, ROOTCert]. The path will be the reverse of the
%% chain and ROOTCert is the trusted anchor in the path validation.
%% Also to complicate matters ROOTCerts can be left out of the sent chain.
%% However due to configuration error and workarounds for certificate
%% renewal purposes we have to handle chains that may be:
%%
%% * Unordered - PeerCert will still be first but the other certs
%% may be arbitrarily order. Ex: [PeerCert, CAN ...  ROOTCert, CA0]
%%
%% * Partial - User decides to put the trust in an intermediate CA
%% that is this intermediate CA must be part of the original chain
%% and is used as the trusted anchor cert instead of the ROOT certificate.
%% Ex: [PeerCert, CA0 ...CAN-1] instead of  [PeerCert, CA0 ...CAN, ROOTCert]
%%
%% * Incomplete - The chain sent is missing one or more certificates
%% but if we have the missing certificates in our trust store we can recreate
%% the chain. Ex:  [PeerCert, CA0 ...CAN-1] and CN and ROOTCert is in our
%% trust store.
%%
%% * Extraneous - Contain extra certificates that are so called cross signed
%% to enable construction of different cert chains depending on what is in the
%% trust store. Used to phase out certificates that are expiring. So that
%% a window can be created when there is an old about to expire cert and
%% a new replacing cert that can coexist.
%% Ex:  [PeerCert, CA0 ...CAN, CAN', ROOTCert]
%%
%% * Cross signed ROOT - Means looking for alternative paths using possible
%% alternative ROOT certs if the original ROOT cert has expired or is unknown. 
%% ----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([trusted_cert_and_paths/4,
         certificate_chain/3,
         certificate_chain/5,
         file_to_certificats/2,
         file_to_crls/2,
         validate/3,
         is_valid_key_usage/2,
         select_extension/2,
         extensions_list/1,
         public_key_type/1,
         foldl_db/3,
         find_cross_sign_root_paths/4,
         handle_cert_auths/4,
         available_cert_key_pairs/1,
         available_cert_key_pairs/2
	]).

%% Tracing
-export([handle_trace/3]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec trusted_cert_and_paths([der_cert()], db_handle(), certdb_ref(), fun()) ->
          [{public_key:combined_cert() | unknown_ca | invalid_issuer | selfsigned_peer, [public_key:combined_cert()]}].
%%
%% Description: Construct input to public_key:pkix_path_validation/3,
%% If the ROOT cert is not found {bad_cert, unknown_ca} will be returned
%% instead of the ROOT cert to be handled as a path validation error
%% by the verify_fun. 
%% Returns {RootCert | RootCertRelatedError, Path} 
%% Note: Path = lists:reverse(Chain) -- Root, that is on the peer cert 
%% always comes first in the chain but last in the path.
%%--------------------------------------------------------------------
trusted_cert_and_paths([Peer],  CertDbHandle, CertDbRef, PartialChainHandler) ->
    OtpCert = public_key:pkix_decode_cert(Peer, otp),
    Chain = [#cert{der=Peer, otp=OtpCert}],
    case public_key:pkix_is_self_signed(OtpCert) of
        true ->
            [{selfsigned_peer, Chain}];
        false ->
            [handle_incomplete_chain(Chain, PartialChainHandler, {unknown_ca, Chain},
                                     CertDbHandle, CertDbRef)]
    end;
trusted_cert_and_paths(Chain0,  CertDbHandle, CertDbRef, PartialChainHandler) ->
    %% Construct possible certificate paths from the chain certificates.
    %% If the chain contains extraneous certificates there could be
    %% more than one possible path such chains might be used to phase out
    %% an old certificate.
    Chain = [#cert{der=Der,otp=public_key:pkix_decode_cert(Der, otp)} || Der <- Chain0],
    Paths = paths(Chain, CertDbHandle),
    lists:map(fun(Path) ->
                      case handle_partial_chain(Path, PartialChainHandler, CertDbHandle, CertDbRef) of
                          {unknown_ca, _} = Result ->
                              handle_incomplete_chain(Chain, 
                                                      PartialChainHandler, 
                                                      Result,
                                                      CertDbHandle, CertDbRef);
                          {_Root, _NewChain} = Result ->
                              Result
                      end
              end, Paths).
%%--------------------------------------------------------------------
-spec certificate_chain([] | binary() | #'OTPCertificate'{} , db_handle(),
                        certdb_ref() | {extracted, list()}) ->
          {error, no_cert} | {ok, der_cert() | undefined, [der_cert()]}.
%%
%% Description: Return the certificate chain to send to peer.
%%--------------------------------------------------------------------
certificate_chain([], _, _) ->
    {error, no_cert};
certificate_chain(DerCert, CertDbHandle, CertsDbRef) when is_binary(DerCert) ->
    ErlCert = public_key:pkix_decode_cert(DerCert, otp),
    Cert = #cert{der=DerCert, otp=ErlCert},
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], []),
    chain_result(Root, Chain, encoded);
certificate_chain(#'OTPCertificate'{} = OtpCert, CertDbHandle, CertsDbRef) ->
    DerCert = public_key:pkix_encode('OTPCertificate', OtpCert, otp),
    Cert = #cert{der=DerCert, otp=OtpCert},
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], []),
    chain_result(Root, Chain, encoded);
certificate_chain(#cert{} = Cert, CertDbHandle, CertsDbRef) -> 
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], []),
    chain_result(Root, Chain, encoded).
%%--------------------------------------------------------------------
-spec certificate_chain(binary() | #'OTPCertificate'{} , db_handle(), certdb_ref() | 
                        {extracted, list()}, [der_cert()], encoded | decoded | both) ->
          {ok,
           der_cert() | #'OTPCertificate'{} | undefined,
           [der_cert() |  #'OTPCertificate'{}]} |
          {ok,
           {der_cert() | undefined,  [der_cert()]},
           {#'OTPCertificate'{} | undefined, [#'OTPCertificate'{}]}
          }.
%%
%% Description: Create certificate chain with certs from Candidates
%%--------------------------------------------------------------------
certificate_chain(DerCert, CertDbHandle, CertsDbRef, Candidates, Type) when is_binary(DerCert) ->
    ErlCert = public_key:pkix_decode_cert(DerCert, otp),
    Cert = #cert{der=DerCert, otp=ErlCert},
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], Candidates),
    chain_result(Root, Chain, Type);
certificate_chain(#'OTPCertificate'{} = OtpCert, CertDbHandle, CertsDbRef, Candidates, Type) ->
    DerCert = public_key:pkix_encode('OTPCertificate', OtpCert, otp),
    Cert = #cert{der=DerCert, otp=OtpCert},
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], Candidates),
    chain_result(Root, Chain, Type);
certificate_chain(#cert{} = Cert, CertDbHandle, CertsDbRef, Candidates, Type) -> 
    {ok, Root, Chain} = build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert], Candidates),
    chain_result(Root, Chain, Type).
                
%%--------------------------------------------------------------------
-spec file_to_certificats(binary(), term()) -> [der_cert()].
%%
%% Description: Return list of DER encoded certificates.
%%--------------------------------------------------------------------
file_to_certificats(File, DbHandle) ->
    {ok, List} = ssl_manager:cache_pem_file(File, DbHandle),
    [Bin || {'Certificate', Bin, not_encrypted} <- List].

%%--------------------------------------------------------------------
-spec file_to_crls(binary(), term()) -> [der_cert()].
%%
%% Description: Return list of DER encoded certificates.
%%--------------------------------------------------------------------
file_to_crls(File, DbHandle) ->
    {ok, List} = ssl_manager:cache_pem_file(File, DbHandle),
    [Bin || {'CertificateList', Bin, not_encrypted} <- List].

%%--------------------------------------------------------------------
-spec validate(term(), {extension, #'Extension'{}} | {bad_cert, atom()} | valid | valid_peer,
	       term()) -> {valid, term()} | {fail, tuple()} | {unknown, term()}.
%%
%% Description:  Validates ssl/tls specific extensions
%%--------------------------------------------------------------------
validate(_,{extension, #'Extension'{extnID = ?'id-ce-extKeyUsage',
                                    critical = Critical,
				    extnValue = KeyUse}}, #{path_len := 1} = UserState) ->
    %% If extension in peer, check for TLS server/client usage
    case is_valid_extkey_usage(KeyUse, Critical, UserState) of
	true ->
	    {valid, UserState};
	false ->
	    {unknown, UserState}
    end;
validate(_, {extension, _}, UserState) ->
    {unknown, UserState};
validate(Issuer, {bad_cert, cert_expired}, #{issuer := Issuer}) ->
    {fail, {bad_cert, root_cert_expired}};
validate(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
validate(Cert, valid, #{path_len := N} = UserState) ->
    case verify_sign(Cert, UserState) of
        true ->
            case maps:get(cert_ext, UserState, undefined) of
                undefined ->
                    {valid, UserState#{path_len => N-1}};
                _ ->
                    verify_cert_extensions(Cert, UserState#{path_len => N-1})
            end;
        false ->
            {fail, {bad_cert, invalid_signature}}
    end;
validate(Cert, valid_peer, UserState = #{role := client, server_name := Hostname, 
                                         customize_hostname_check := Customize}) when Hostname =/= disable ->
    case verify_hostname(Hostname, Customize, Cert, UserState) of
        {valid, UserState} ->
            validate(Cert, valid, UserState);
        Error ->
            Error
    end;
validate(Cert, valid_peer, UserState) ->    
    validate(Cert, valid, UserState).

%%--------------------------------------------------------------------
-spec is_valid_key_usage(list(), term()) -> boolean().
%%
%% Description: Checks if Use is a valid key usage.
%%--------------------------------------------------------------------
is_valid_key_usage(KeyUse, Use) ->
    lists:member(Use, KeyUse).
 
%%--------------------------------------------------------------------
-spec select_extension(term(), list()) -> undefined | #'Extension'{}.
%%
%% Description: Selects the extension identified by Id if present in
%% a list of extensions.
%%--------------------------------------------------------------------
select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

%%--------------------------------------------------------------------
-spec extensions_list(asn1_NOVALUE | list()) -> list().
%%
%% Description: Handles that 
%%--------------------------------------------------------------------
extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.

%%--------------------------------------------------------------------
-spec public_key_type(term()) -> rsa | rsa_pss_pss | dsa | ecdsa | eddsa.
%%
%% Description:
%%--------------------------------------------------------------------
public_key_type(?'id-RSASSA-PSS') ->
    rsa_pss_pss;
public_key_type(?'rsaEncryption') ->
    rsa;
public_key_type(?'id-dsa') ->
    dsa;
public_key_type(?'id-ecPublicKey') ->
    ecdsa;
public_key_type(Oid) ->
    {_, Sign} = public_key:pkix_sign_types(Oid),
    Sign.

%%--------------------------------------------------------------------
-spec foldl_db(fun(), db_handle() | {extracted, list()}, list()) ->
 {ok, term()} | issuer_not_found.
%%
%% Description:
%%--------------------------------------------------------------------
foldl_db(IsIssuerFun, CertDbHandle, []) ->
    ssl_pkix_db:foldl(IsIssuerFun, issuer_not_found, CertDbHandle);
foldl_db(IsIssuerFun, _, [_|_] = ListDb) ->
    lists:foldl(IsIssuerFun, issuer_not_found, ListDb).

find_cross_sign_root_paths([], _CertDbHandle, _CertDbRef, _InvalidatedList) ->
    [];
find_cross_sign_root_paths([_ | Rest] = Path, CertDbHandle, CertDbRef, InvalidatedList) ->
    case find_alternative_root(Path, CertDbHandle, CertDbRef, InvalidatedList) of
        unknown_ca ->
            find_cross_sign_root_paths(Rest, CertDbHandle, CertDbRef, InvalidatedList);
        Root ->
            [{Root, Path}]
    end.

handle_cert_auths(Chain, [], _, _) ->
    %% If we have no authorities extension (or corresponding
    %% 'certificate_authorities' in the certificate request message in
    %% TLS-1.2 is empty) to check we just accept first choice.
    {ok, Chain};
handle_cert_auths([Cert], CertAuths, CertDbHandle, CertDbRef) ->
    case certificate_chain(Cert, CertDbHandle, CertDbRef, [], both) of
        {ok, {_, [Cert | _] = EChain}, _}  ->
            case cert_auth_member(cert_issuers(EChain), CertAuths) of
                true ->
                    {ok, EChain};
                false ->
                    {error, EChain, not_in_auth_domain}
            end;
        _ ->
            {ok, [Cert]}
    end;
handle_cert_auths([_ | _] = EChain, CertAuths, _, _) ->
    case cert_auth_member(cert_issuers(EChain), CertAuths) of
        true ->
            {ok, EChain};
        false ->
            {error, EChain, not_in_auth_domain}
    end.

available_cert_key_pairs(CertKeyGroups) ->
    %% To be able to find possible TLS session pre TLS-1.3
    %% that may be reused. At this point the version is
    %% not negotiated.
    RevAlgos = [dsa, rsa, rsa_pss_pss, ecdsa],
    cert_key_group_to_list(RevAlgos, CertKeyGroups, []).

%% Create the prioritized list of cert key pairs that
%% are availble for use in the negotiated version
available_cert_key_pairs(CertKeyGroups, ?TLS_1_3) ->
    RevAlgos = [rsa, rsa_pss_pss, ecdsa, eddsa],
    cert_key_group_to_list(RevAlgos, CertKeyGroups, []);
available_cert_key_pairs(CertKeyGroups, ?TLS_1_2) ->
     RevAlgos = [dsa, rsa, rsa_pss_pss, ecdsa],
    cert_key_group_to_list(RevAlgos, CertKeyGroups, []);
available_cert_key_pairs(CertKeyGroups, Version)
  when ?TLS_LT(Version, ?TLS_1_2) ->
    RevAlgos = [dsa, rsa, ecdsa],
    cert_key_group_to_list(RevAlgos, CertKeyGroups, []).

cert_key_group_to_list([], _, Acc) ->
    final_group_list(Acc);
cert_key_group_to_list([Algo| Rest], CertKeyGroups, Acc) ->
    CertKeyPairs = maps:get(Algo, CertKeyGroups, []),
    cert_key_group_to_list(Rest, CertKeyGroups, CertKeyPairs ++ Acc).

final_group_list([]) ->
    [#{certs => [[]], private_key => #{}}];
final_group_list(List) ->
    List.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encoded_chain(#cert{der=Cert}, Certs) ->
    {Cert, [C || #cert{der=C} <- Certs]};
encoded_chain(Res, Certs) ->
    {Res, [OtpC || #cert{der=OtpC} <- Certs]}.

decoded_chain(#cert{otp=OtpCert}, Certs) ->
    {OtpCert, [OtpC || #cert{otp=OtpC} <- Certs]};
decoded_chain(Res, Certs) ->
    {Res, [OtpC || #cert{otp=OtpC} <- Certs]}.

chain_result(Root0, Chain0, encoded) ->
    {Root, Chain} = encoded_chain(Root0, Chain0),
    {ok, Root, Chain};
chain_result(Root0, Chain0, decoded) ->
    {Root, Chain} = decoded_chain(Root0, Chain0),
    {ok, Root, Chain};
chain_result(Root0, Chain0, both) ->
    {ERoot, EChain} = encoded_chain(Root0, Chain0),
    {DRoot, DChain} = decoded_chain(Root0, Chain0),
    {ok, {ERoot, EChain}, {DRoot, DChain}}.

build_certificate_chain(#cert{otp=OtpCert}=Cert, CertDbHandle, CertsDbRef, Chain, ListDb) ->
    IssuerAndSelfSigned = 
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{public_key:pkix_issuer_id(OtpCert, self), true};
	    false  ->
		{public_key:pkix_issuer_id(OtpCert, other), false}
	end,
    case IssuerAndSelfSigned of
	{_, true = SelfSigned} ->
	    do_certificate_chain(CertDbHandle, CertsDbRef, Chain, ignore, ignore, SelfSigned, ListDb);
	{{error, issuer_not_found}, SelfSigned} ->
	    case find_issuer(Cert, CertDbHandle, CertsDbRef, ListDb, []) of
		{ok, {SerialNr, Issuer}} ->
		    do_certificate_chain(CertDbHandle, CertsDbRef, Chain,
					 SerialNr, Issuer, SelfSigned, ListDb);
		_Err ->
		    %% Guess the the issuer must be the root
		    %% certificate. The verification of the
		    %% cert chain will fail if guess is
		    %% incorrect.
		    {ok, undefined, lists:reverse(Chain)}
	    end;
	{{ok, {SerialNr, Issuer}}, SelfSigned} -> 
	    do_certificate_chain(CertDbHandle, CertsDbRef, Chain, SerialNr, Issuer, SelfSigned, ListDb)
    end.

do_certificate_chain(_, _, [RootCert | _] = Chain, _, _, true, _) ->
    {ok, RootCert, lists:reverse(Chain)};

do_certificate_chain(CertDbHandle, CertsDbRef, Chain, SerialNr, Issuer, _, ListDb) ->
    case ssl_manager:lookup_trusted_cert(CertDbHandle, CertsDbRef,
                                         SerialNr, Issuer) of
	{ok, Cert} ->
	    build_certificate_chain(Cert, CertDbHandle, CertsDbRef, [Cert | Chain], ListDb);
	_ ->
	    %% The trusted cert may be obmitted from the chain as the
	    %% counter part needs to have it anyway to be able to
	    %% verify it.
	    {ok, undefined, lists:reverse(Chain)}
    end.

find_alternative_root([Cert | _], CertDbHandle, CertDbRef, InvalidatedList) ->
    case find_issuer(Cert, CertDbHandle, CertDbRef, [], InvalidatedList) of
        {error, issuer_not_found} ->
            unknown_ca;
        {ok, {SerialNr, IssuerId}} ->
            case ssl_manager:lookup_trusted_cert(CertDbHandle, CertDbRef, SerialNr, IssuerId) of
                undefined ->
                    unknown_ca;
                {ok, #cert{otp = OtpIssuer}} ->
                    case public_key:pkix_is_self_signed(OtpIssuer) of
                        true ->
                            OtpIssuer;
                        false ->
                            unknown_ca
                    end
            end
    end.

find_issuer(#cert{der=DerCert, otp=OtpCert}, CertDbHandle, CertsDbRef, ListDb, InvalidatedList) ->
   IsIssuerFun =
	fun({_Key, #cert{otp=ErlCertCandidate}}, Acc) ->
		case public_key:pkix_is_issuer(OtpCert, ErlCertCandidate) of
		    true ->
			case verify_cert_signer(DerCert, ErlCertCandidate#'OTPCertificate'.tbsCertificate)
                            andalso (not lists:member(ErlCertCandidate, InvalidatedList))
                        of
			    true ->
				throw(public_key:pkix_issuer_id(ErlCertCandidate, self));
			    false ->
				Acc
			end;
		    false ->
			Acc
		end;
	   (_, Acc) ->
		Acc
	end,

    Result = case is_reference(CertsDbRef) of
		 true when ListDb == [] ->
                     CertEntryList = ssl_pkix_db:select_certentries_by_ref(CertsDbRef, CertDbHandle),
		     do_find_issuer(IsIssuerFun, CertDbHandle, CertEntryList); 
		 false when ListDb == [] ->
		     {extracted, CertsData} = CertsDbRef,
		     CertEntryList = [Entry || {decoded, Entry} <- CertsData],
		     do_find_issuer(IsIssuerFun, CertDbHandle, CertEntryList);
                 _ ->
                     do_find_issuer(IsIssuerFun, CertDbHandle, ListDb)
	     end,
    case Result of
        issuer_not_found ->
	    {error, issuer_not_found};
	Result ->
	    Result
    end.


do_find_issuer(IssuerFun, CertDbHandle, CertDb) ->
    try 
	foldl_db(IssuerFun, CertDbHandle, CertDb)
    catch
	throw:{ok, _} = Return ->
	    Return
    end.

is_valid_extkey_usage(KeyUse, true, #{role := Role}) when is_list(KeyUse) ->
    is_valid_key_usage(KeyUse, ext_keysage(Role));
is_valid_extkey_usage(KeyUse, true, #{role := Role}) ->
    is_valid_key_usage([KeyUse], ext_keysage(Role));
is_valid_extkey_usage(_, false, _) ->
    false.

ext_keysage(client) ->
    %% Client wants to verify server
    ?'id-kp-serverAuth';
ext_keysage(server) ->
    %% Server wants to verify client
    ?'id-kp-clientAuth'.

verify_cert_signer(BinCert, SignerTBSCert) ->
    PublicKey = public_key(SignerTBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo),
    public_key:pkix_verify(BinCert, PublicKey).

public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
									parameters = Params},
				      subjectPublicKey = Point}) ->
    {Point, Params};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-Ed25519'},
				      subjectPublicKey = Point}) ->
    {Point, {namedCurve, ?'id-Ed25519'}};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-Ed448'},
				      subjectPublicKey = Point}) ->
    {Point, {namedCurve, ?'id-Ed448'}};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'rsaEncryption'}, 
				      subjectPublicKey = Key}) ->
    Key;
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                                                        parameters = Params}, 
				      subjectPublicKey = Key}) ->
    {Key, Params};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-dsa',
									parameters = {params, Params}},
				      subjectPublicKey = Key}) ->
    {Key, Params}.

other_issuer(#cert{otp=OtpCert}=Cert, CertDbHandle, CertDbRef) ->
    case public_key:pkix_issuer_id(OtpCert, other) of
	{ok, IssuerId} ->
	    {other, IssuerId};
	{error, issuer_not_found} ->
	    case find_issuer(Cert, CertDbHandle, CertDbRef, [], []) of
		{ok, IssuerId} ->
		    {other, IssuerId};
		Other ->
		    Other
	    end
    end.

verify_hostname(Hostname, Customize, Cert, UserState) when is_tuple(Hostname) ->
    case public_key:pkix_verify_hostname(Cert, [{ip, Hostname}], Customize) of
        true  -> {valid, UserState};
        false -> {fail, {bad_cert, hostname_check_failed}}
    end;
verify_hostname(Hostname, Customize, Cert, UserState) ->
    HostId = case inet:parse_strict_address(Hostname) of
                 {ok, IP} -> {ip, IP};
                 _ -> {dns_id, Hostname}
             end,
    case public_key:pkix_verify_hostname(Cert, [HostId], Customize) of
        true  -> {valid, UserState};
        false -> {fail, {bad_cert, hostname_check_failed}}
    end.

verify_cert_extensions(Cert, #{cert_ext := CertExts} =  UserState) ->
    Id = public_key:pkix_subject_id(Cert),
    Extensions = maps:get(Id, CertExts, []),
    verify_cert_extensions(Cert, UserState, Extensions, #{}).

verify_cert_extensions(Cert, UserState, [], _) ->
    {valid, UserState#{issuer => Cert}};
verify_cert_extensions(Cert, #{ocsp_responder_certs := ResponderCerts,
                               ocsp_state := OscpState,
                               issuer := Issuer} = UserState,
                       [#certificate_status{response = OcspResponsDer} | Exts],
                       Context) ->
    #{ocsp_nonce := Nonce} = OscpState,
    case public_key:pkix_ocsp_validate(Cert, Issuer, OcspResponsDer,
                                       ResponderCerts, Nonce) of
        valid ->
            verify_cert_extensions(Cert, UserState, Exts, Context);
        {bad_cert, _} = Status ->
            {fail, Status}
    end;
verify_cert_extensions(Cert, UserState, [_|Exts], Context) ->
    %% Skip unknown extensions!
    verify_cert_extensions(Cert, UserState, Exts, Context).

verify_sign(_, #{version := Version})
            when ?TLS_LT(Version, ?TLS_1_2) ->
    %% This verification is not applicable pre TLS-1.2 
    true; 
verify_sign(Cert, #{version := ?TLS_1_2,
                    signature_algs := SignAlgs,
                    signature_algs_cert := undefined}) ->
    is_supported_signature_algorithm_1_2(Cert, SignAlgs);
verify_sign(Cert, #{version := ?TLS_1_2,
                    signature_algs_cert := SignAlgs}) ->
    is_supported_signature_algorithm_1_2(Cert, SignAlgs);
verify_sign(Cert, #{version := ?TLS_1_3,
                    signature_algs := SignAlgs,
                    signature_algs_cert := undefined}) ->
    is_supported_signature_algorithm_1_3(Cert, SignAlgs);
verify_sign(Cert, #{version := ?TLS_1_3,
                    signature_algs_cert := SignAlgs}) ->
    is_supported_signature_algorithm_1_3(Cert, SignAlgs).

is_supported_signature_algorithm_1_2(#'OTPCertificate'{signatureAlgorithm =
                                                           #'SignatureAlgorithm'{algorithm = ?'id-dsa-with-sha1'}},
                                     SignAlgs) ->
    lists:member({sha, dsa}, SignAlgs);
is_supported_signature_algorithm_1_2(#'OTPCertificate'{signatureAlgorithm =
                                                           #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS'}} = Cert,
                                     SignAlgs) ->
    is_supported_signature_algorithm_1_3(Cert, SignAlgs);
is_supported_signature_algorithm_1_2(#'OTPCertificate'{signatureAlgorithm = SignAlg}, SignAlgs) ->
    Scheme = ssl_cipher:signature_algorithm_to_scheme(SignAlg),
    {Hash, Sign, _ } = ssl_cipher:scheme_to_components(Scheme),
    ssl_cipher:is_supported_sign({Hash, pre_1_3_sign(Sign)}, ssl_cipher:signature_schemes_1_2(SignAlgs)).
is_supported_signature_algorithm_1_3(#'OTPCertificate'{signatureAlgorithm = SignAlg}, SignAlgs) ->
    Scheme = ssl_cipher:signature_algorithm_to_scheme(SignAlg),
    ssl_cipher:is_supported_sign(Scheme, SignAlgs).

pre_1_3_sign(rsa_pkcs1) ->
    rsa;
pre_1_3_sign(Other) ->
    Other.

paths(Chain, CertDbHandle) ->
    paths(Chain, Chain, CertDbHandle, []).

paths([Root], _, _, Path) ->
    [[Root | Path]];
paths([#cert{otp=C1}=Cert1, #cert{otp=C2}=Cert2 | Rest], Chain, CertDbHandle, Path) ->
    case public_key:pkix_is_issuer(C1, C2) of
        true ->
            %% Chain ordered so far
            paths([Cert2 | Rest], Chain, CertDbHandle, [Cert1 | Path]);
        false ->
            %% Chain is unorded and/or contains extraneous certificates
            unorded_or_extraneous(Chain, CertDbHandle)
    end.

unorded_or_extraneous([Peer | UnorderedChain], CertDbHandle) ->
    ChainCandidates = extraneous_chains(UnorderedChain),
    lists:map(fun(Candidate) ->
                      path_candidate(Peer, Candidate, CertDbHandle)
              end,
              ChainCandidates).

path_candidate(Cert, ChainCandidateCAs, CertDbHandle) ->
    {ok,  ExtractedCerts} = ssl_pkix_db:extract_trusted_certs({der_otp, ChainCandidateCAs}),
    %% certificate_chain/4 will make sure the chain is ordered
    case build_certificate_chain(Cert, CertDbHandle, ExtractedCerts, [Cert], []) of
        {ok, undefined, Chain} ->
            lists:reverse(Chain);
        {ok, Root, Chain} ->
            [Root | lists:reverse(Chain)]
    end.

handle_partial_chain([#cert{der=DERIssuerCert, otp=OtpIssuerCert}=Cert| Rest] = Path, PartialChainHandler,
                     CertDbHandle, CertDbRef) ->
    case public_key:pkix_is_self_signed(OtpIssuerCert) of
        true -> %% IssuerCert = ROOT (That is ROOT was included in chain)
            {ok, {SerialNr, IssuerId}} = public_key:pkix_issuer_id(OtpIssuerCert, self),
            case ssl_manager:lookup_trusted_cert(CertDbHandle, CertDbRef, SerialNr, IssuerId) of
                {ok, #cert{der=DERIssuerCert}} -> %% Match sent ROOT to trusted ROOT
                    maybe_shorten_path(Path, PartialChainHandler, {Cert, Rest});
                {ok, _} -> %% Did not match trusted ROOT
                    maybe_shorten_path(Path, PartialChainHandler, {invalid_issuer, Path});
                _ ->
                    maybe_shorten_path(Path, PartialChainHandler, {unknown_ca, Path})
            end;
        false ->
            case other_issuer(Cert, CertDbHandle, CertDbRef) of
                {other, {SerialNr, IssuerId}} ->
                    case ssl_manager:lookup_trusted_cert(CertDbHandle, CertDbRef, SerialNr, IssuerId) of
                        {ok, #cert{otp=NewOtp}=NewCert} ->
                            case public_key:pkix_is_self_signed(NewOtp) of
                                true -> %% NewIssuerCert is a trusted ROOT cert
                                    maybe_shorten_path([NewCert | Path], PartialChainHandler, {NewCert, Path});
                                false ->
                                    maybe_shorten_path([NewCert | Path], PartialChainHandler, 
                                                       {unknown_ca, [NewCert | Path]})
                            end;
                        _ ->
                            maybe_shorten_path(Path, PartialChainHandler, {unknown_ca, Path})
                    end;
                {error, issuer_not_found} ->
                    maybe_shorten_path(Path, PartialChainHandler, {unknown_ca, Path})
            end
    end. 

maybe_shorten_path(Path, PartialChainHandler, Default) ->
    %% This function might shorthen the
    %% certificate path to be validated with
    %% public_key:pkix_path_validation by letting
    %% the user put its trust in an intermidate cert
    %% from the certificate chain sent by the peer.
    DerCerts = [Der || #cert{der=Der} <- Path],
    try PartialChainHandler(DerCerts) of
        {trusted_ca, Root} ->
            new_trusted_path(Root, Path, Default);
        unknown_ca ->
            Default
    catch _:_ ->
            Default
    end.

new_trusted_path(DerCert, [#cert{der=DerCert}=Cert | Path], _) ->
    {Cert, Path};
new_trusted_path(DerCert, [_ | Rest], Default) ->
    new_trusted_path(DerCert, Rest, Default);
new_trusted_path(_, [], Default) ->
    %% User did not pick a cert present 
    %% in the cert chain so ignore
    Default.

handle_incomplete_chain([#cert{}=Peer| _] = Chain0, PartialChainHandler, Default, CertDbHandle, CertDbRef) ->
    %% We received an incomplete chain, that is not all certs expected to be present are present.
    %% See if we have the certificates to rebuild it.
    case build_certificate_chain(Peer, CertDbHandle, CertDbRef, [Peer], []) of
        {ok, _, [Peer | _] = ChainCandidate} when ChainCandidate =/= Chain0 -> %% Chain candidate found
            case lists:prefix(Chain0, ChainCandidate) of
                true ->
                    {Root, Chain} = handle_partial_chain(lists:reverse(ChainCandidate), PartialChainHandler, 
                                                         CertDbHandle, CertDbRef),
                    {Root, Chain};
                false ->
                    Default
            end;
        _  ->
            Default
    end.

extraneous_chains(Certs) ->
    %% If some certs claim to be the same cert that is have the same
    %% subject field we should create a list of possible chain certs
    %% for each such cert. Only one chain, if any, should be
    %% verifiable using available ROOT certs.
    Subjects = [{subject(OTP), Cert} || #cert{otp=OTP} = Cert <- Certs],
    Duplicates = find_duplicates(Subjects),
    %% Number of certs with duplicates (same subject) has been limited
    %% to 4 and the maximum number of combinations is limited to 16.
    build_candidates(Duplicates, 4, 16).

build_candidates(Map, Duplicates, Combinations) ->
    Subjects = maps:keys(Map),
    build_candidates(Subjects, Map, Duplicates, 1, Combinations, []).
%%
build_candidates([], _, _, _, _, Acc) ->
    Acc;
build_candidates([H|T], Map, Duplicates, Combinations, Max, Acc0) ->
    case maps:get(H, Map) of
	{Certs, Counter} when Counter > 1 andalso
                              Duplicates > 0 andalso
                              Counter * Combinations =< Max ->
	    case Acc0 of
		[] ->
		    Acc = [[Cert] || Cert <- Certs],
		    build_candidates(T, Map, Duplicates - 1, Combinations * Counter, Max, Acc);
		_Else ->
		    Acc = [[Cert|L] || Cert <- Certs, L <- Acc0],
		    build_candidates(T, Map, Duplicates - 1, Combinations * Counter, Max, Acc)
            end;
	{[Cert|_Throw], _Counter} ->
	    case Acc0 of
		[] ->
		    Acc = [[Cert]],
		    build_candidates(T, Map, Duplicates, Combinations, Max, Acc);
		_Else ->
		    Acc = [[Cert|L] || L <- Acc0],
		    build_candidates(T, Map, Duplicates, Combinations, Max, Acc)
	    end
    end.

find_duplicates(Chain) ->
    find_duplicates(Chain, #{}).
%%
find_duplicates([], Acc) ->
    Acc;
find_duplicates([{Subject, Cert}|T], Acc) ->
    case maps:get(Subject, Acc, none) of
	none ->
	    find_duplicates(T, Acc#{Subject => {[Cert], 1}});
	{Certs, Counter} ->
	    find_duplicates(T, Acc#{Subject => {[Cert|Certs], Counter + 1}})
    end.

subject(Cert) ->
    {_Serial,Subject} = public_key:pkix_subject_id(Cert),
    Subject.

issuer(Cert) ->
    case public_key:pkix_is_self_signed(Cert) of
        true ->
            subject(Cert);
        false ->
            case is_binary(Cert) of
                true ->
                    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
                    public_key:pkix_normalize_name(TBSCert#'OTPTBSCertificate'.issuer);
                false ->
                    #'OTPCertificate'{tbsCertificate = TBSCert} = Cert,
                    public_key:pkix_normalize_name(TBSCert#'OTPTBSCertificate'.issuer)
            end
    end.

cert_issuers([], Acc) ->
    Acc;
cert_issuers([Cert | Rest], Acc) ->
    cert_issuers(Rest, [issuer(Cert) | Acc]).

cert_issuers(OTPCerts) ->
    cert_issuers(OTPCerts, []).

cert_auth_member(ChainSubjects, CertAuths) ->
    CommonAuthorities = sets:intersection(sets:from_list(ChainSubjects), sets:from_list(CertAuths)),
    not sets:is_empty(CommonAuthorities).

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(crt,
             {call, {?MODULE, validate, [Cert, StatusOrExt| _]}}, Stack) ->
    {io_lib:format("[~W] StatusOrExt = ~W", [Cert, 3, StatusOrExt, 10]), Stack};
    %% {io_lib:format("(~s) StatusOrExt = ~W",
    %%                [ssl_test_lib:format_cert(Cert), StatusOrExt, 10]), Stack};
handle_trace(crt, {call, {?MODULE, verify_cert_extensions,
                          [Cert,
                           _UserState,
                           [], _Context]}}, Stack) ->
    {io_lib:format(" no more extensions [~W]", [Cert, 3]), Stack};
    %% {io_lib:format(" no more extensions (~s)", [ssl_test_lib:format_cert(Cert)]), Stack};
handle_trace(crt, {call, {?MODULE, verify_cert_extensions,
                          [Cert,
                           #{ocsp_responder_certs := _ResponderCerts,
                             ocsp_state := OcspState,
                             issuer := Issuer} = _UserState,
                           [#certificate_status{response = OcspResponsDer} |
                            _Exts], _Context]}}, Stack) ->
    {io_lib:format("#2 OcspState = ~W Issuer = [~W] OcspResponsDer = ~W [~W]",
                   [OcspState, 10, Issuer, 3, OcspResponsDer, 2, Cert, 3]),
     Stack};
    %% {io_lib:format("#2 OcspState = ~W Issuer = (~s) OcspResponsDer = ~W (~s)",
    %%                [OcspState, 10, ssl_test_lib:format_cert(Issuer),
    %%                 OcspResponsDer, 2, ssl_test_lib:format_cert(Cert)]),
handle_trace(crt, {return_from,
                   {ssl_certificate, verify_cert_extensions, 4},
                   {valid, #{issuer := Issuer}}}, Stack) ->
    {io_lib:format(" extensions valid Issuer = ~W", [Issuer, 3]), Stack}.
    %% {io_lib:format(" extensions valid Issuer = ~s", [ssl_test_lib:format_cert(Issuer)]), Stack}.
