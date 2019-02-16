%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018 All Rights Reserved.
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
%% Purpose: Help funtions for handling certificat verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%%----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([trusted_cert_and_path/4,
	 certificate_chain/3,
	 certificate_chain/4,
	 file_to_certificats/2,
	 file_to_crls/2,
	 validate/3,
	 is_valid_extkey_usage/2,
	 is_valid_key_usage/2,
	 select_extension/2,
	 extensions_list/1,
	 public_key_type/1,
	 foldl_db/3
	]).
 
%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec trusted_cert_and_path([der_cert()], db_handle(), certdb_ref(), fun()) ->
				   {der_cert() | unknown_ca, [der_cert()]}.
%%
%% Description: Extracts the root cert (if not presents tries to 
%% look it up, if not found {bad_cert, unknown_ca} will be added verification
%% errors. Returns {RootCert, Path, VerifyErrors}
%%--------------------------------------------------------------------
trusted_cert_and_path(CertChain, CertDbHandle, CertDbRef, PartialChainHandler) ->
    Path = [BinCert | _] = lists:reverse(CertChain),
    OtpCert = public_key:pkix_decode_cert(BinCert, otp),
    SignedAndIssuerID =
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{ok, IssuerId} = public_key:pkix_issuer_id(OtpCert, self),
		{self, IssuerId};
	    false ->
		other_issuer(OtpCert, BinCert, CertDbHandle, CertDbRef)
	end,
    
    case SignedAndIssuerID of
	{error, issuer_not_found} ->
	    %% The root CA was not sent and cannot be found.
	    handle_incomplete_chain(Path, PartialChainHandler);
	{self, _} when length(Path) == 1 ->
	    {selfsigned_peer, Path};
	{_ ,{SerialNr, Issuer}} ->
	    case ssl_manager:lookup_trusted_cert(CertDbHandle, CertDbRef, SerialNr, Issuer) of
		{ok, Trusted} ->
		    %% Trusted must be selfsigned or it is an incomplete chain
		    handle_path(Trusted, Path, PartialChainHandler);
		_ ->
		    %% Root CA could not be verified, but partial
                    %% chain handler may trusted a cert that we got
		    handle_incomplete_chain(Path, PartialChainHandler)
	    end
    end.

%%--------------------------------------------------------------------
-spec certificate_chain(undefined | binary() | #'OTPCertificate'{} , db_handle(), certdb_ref()) ->
			  {error, no_cert} | {ok, #'OTPCertificate'{} | undefined, [der_cert()]}.
%%
%% Description: Return the certificate chain to send to peer.
%%--------------------------------------------------------------------
certificate_chain(undefined, _, _) ->
    {error, no_cert};
certificate_chain(OwnCert, CertDbHandle, CertsDbRef) when is_binary(OwnCert) ->
    ErlCert = public_key:pkix_decode_cert(OwnCert, otp),
    certificate_chain(ErlCert, OwnCert, CertDbHandle, CertsDbRef, [OwnCert], []);
certificate_chain(OwnCert, CertDbHandle, CertsDbRef) ->
    DerCert = public_key:pkix_encode('OTPCertificate', OwnCert, otp),
    certificate_chain(OwnCert, DerCert, CertDbHandle, CertsDbRef, [DerCert], []).

%%--------------------------------------------------------------------
-spec certificate_chain(undefined | binary() | #'OTPCertificate'{} , db_handle(), certdb_ref(), [der_cert()]) ->
			  {error, no_cert} | {ok, #'OTPCertificate'{} | undefined, [der_cert()]}.
%%
%% Description: Create certificate chain with certs from 
%%--------------------------------------------------------------------
certificate_chain(Cert, CertDbHandle, CertsDbRef, Candidates) when is_binary(Cert) ->
    ErlCert = public_key:pkix_decode_cert(Cert, otp),
    certificate_chain(ErlCert, Cert, CertDbHandle, CertsDbRef, [Cert], Candidates);
certificate_chain(Cert, CertDbHandle, CertsDbRef, Candidates) ->
    DerCert = public_key:pkix_encode('OTPCertificate', Cert, otp),
    certificate_chain(Cert, DerCert, CertDbHandle, CertsDbRef, [DerCert], Candidates).
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
-spec validate(term(), {extension, #'Extension'{}} | {bad_cert, atom()} | valid,
	       term()) -> {valid, term()} |
			  {fail, tuple()} |
			  {unknown, term()}.
%%
%% Description:  Validates ssl/tls specific extensions
%%--------------------------------------------------------------------
validate(_,{extension, #'Extension'{extnID = ?'id-ce-extKeyUsage',
				    extnValue = KeyUse}}, UserState = {Role, _,_, _, _, _}) ->
    case is_valid_extkey_usage(KeyUse, Role) of
	true ->
	    {valid, UserState};
	false ->
	    {fail, {bad_cert, invalid_ext_key_usage}}
    end;
validate(_, {extension, _}, UserState) ->
    {unknown, UserState};
validate(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
validate(_, valid, UserState) ->
    {valid, UserState};
validate(Cert, valid_peer, UserState = {client, _,_, {Hostname, Customize}, _, _}) when Hostname =/= disable ->
    verify_hostname(Hostname, Customize, Cert, UserState);  
validate(_, valid_peer, UserState) ->    
   {valid, UserState}.

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
-spec public_key_type(term()) -> rsa | dsa | ec.
%%
%% Description:
%%--------------------------------------------------------------------
public_key_type(?'rsaEncryption') ->
    rsa;
public_key_type(?'id-dsa') ->
    dsa;
public_key_type(?'id-ecPublicKey') ->
    ec.

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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_chain(OtpCert, BinCert, CertDbHandle, CertsDbRef, Chain, ListDb) ->
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
	    case find_issuer(OtpCert, BinCert, CertDbHandle, CertsDbRef, ListDb) of
		{ok, {SerialNr, Issuer}} ->
		    do_certificate_chain(CertDbHandle, CertsDbRef, Chain,
					 SerialNr, Issuer, SelfSigned, ListDb);
		_ ->
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
	{ok, {IssuerCert, ErlCert}} ->
	    ErlCert = public_key:pkix_decode_cert(IssuerCert, otp),
	    certificate_chain(ErlCert, IssuerCert, 
			      CertDbHandle, CertsDbRef, [IssuerCert | Chain], ListDb);
	_ ->
	    %% The trusted cert may be obmitted from the chain as the
	    %% counter part needs to have it anyway to be able to
	    %% verify it.
	    {ok, undefined, lists:reverse(Chain)}		      
    end.


find_issuer(OtpCert, BinCert, CertDbHandle, CertsDbRef, ListDb) ->
    IsIssuerFun =
	fun({_Key, {_Der, #'OTPCertificate'{} = ErlCertCandidate}}, Acc) ->
		case public_key:pkix_is_issuer(OtpCert, ErlCertCandidate) of
		    true ->
			case verify_cert_signer(BinCert, ErlCertCandidate#'OTPCertificate'.tbsCertificate) of
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
		 true ->
		     do_find_issuer(IsIssuerFun, CertDbHandle, ListDb); 
		 false ->
		     {extracted, CertsData} = CertsDbRef,
		     DB = [Entry || {decoded, Entry} <- CertsData],
		     do_find_issuer(IsIssuerFun, CertDbHandle, DB)
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
	
is_valid_extkey_usage(KeyUse, client) ->
    %% Client wants to verify server
    is_valid_key_usage(KeyUse,?'id-kp-serverAuth');
is_valid_extkey_usage(KeyUse, server) ->
    %% Server wants to verify client
    is_valid_key_usage(KeyUse, ?'id-kp-clientAuth').

verify_cert_signer(BinCert, SignerTBSCert) ->
    PublicKey = public_key(SignerTBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo),
    public_key:pkix_verify(BinCert, PublicKey).

public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
									parameters = Params},
				      subjectPublicKey = Point}) ->
    {Point, Params};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'rsaEncryption'}, 
				      subjectPublicKey = Key}) ->
    Key;
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-dsa',
									parameters = {params, Params}},
				      subjectPublicKey = Key}) ->
    {Key, Params}.

other_issuer(OtpCert, BinCert, CertDbHandle, CertDbRef) ->
    case public_key:pkix_issuer_id(OtpCert, other) of
	{ok, IssuerId} ->
	    {other, IssuerId};
	{error, issuer_not_found} ->
	    case find_issuer(OtpCert, BinCert, CertDbHandle, CertDbRef, []) of
		{ok, IssuerId} ->
		    {other, IssuerId};
		Other ->
		    Other
	    end
    end.

handle_path({BinCert, OTPCert}, Path, PartialChainHandler) ->
    case public_key:pkix_is_self_signed(OTPCert) of
	true ->
	    {BinCert, lists:delete(BinCert, Path)};
	false ->
	   handle_incomplete_chain(Path, PartialChainHandler)
    end.

handle_incomplete_chain(Chain, Fun) ->
    case catch Fun(Chain) of
	{trusted_ca, DerCert} ->
	    new_trusteded_chain(DerCert, Chain);
	unknown_ca = Error ->
	    {Error, Chain};
	_  ->
	    {unknown_ca, Chain}
    end.

new_trusteded_chain(DerCert, [DerCert | Chain]) ->
    {DerCert, Chain};
new_trusteded_chain(DerCert, [_ | Rest]) ->
    new_trusteded_chain(DerCert, Rest);
new_trusteded_chain(_, []) ->
    unknown_ca.

verify_hostname({fallback, Hostname}, Customize, Cert, UserState) when is_list(Hostname) ->
    case public_key:pkix_verify_hostname(Cert, [{dns_id, Hostname}], Customize) of
        true ->
            {valid, UserState};
        false ->
            case public_key:pkix_verify_hostname(Cert, [{ip, Hostname}], Customize) of
                true ->
                    {valid, UserState};
                false ->
                    {fail, {bad_cert, hostname_check_failed}}
            end
    end;

verify_hostname({fallback, Hostname}, Customize, Cert, UserState) ->
    case public_key:pkix_verify_hostname(Cert, [{ip, Hostname}], Customize) of
        true ->
            {valid, UserState};
        false ->
            {fail, {bad_cert, hostname_check_failed}}
    end;

verify_hostname(Hostname, Customize, Cert, UserState) ->
    case public_key:pkix_verify_hostname(Cert, [{dns_id, Hostname}], Customize) of
        true ->
            {valid, UserState};
        false ->
            {fail, {bad_cert, hostname_check_failed}}
    end.
