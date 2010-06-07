%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling certificat verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%%----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_debug.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([trusted_cert_and_path/3, 
	 certificate_chain/2, 
	 file_to_certificats/1,
	 validate_extensions/6,
	 is_valid_extkey_usage/2,
	 is_valid_key_usage/2,
	 select_extension/2,
	 extensions_list/1,
	 signature_type/1
	]).
 
%%====================================================================
%% Internal application API
%%====================================================================

trusted_cert_and_path(CertChain, CertDbRef, Verify) ->
    [Cert | RestPath] = lists:reverse(CertChain),
    {ok, OtpCert} = public_key:pkix_decode_cert(Cert, otp),
    IssuerAnPath = 
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{ok, IssuerId} = public_key:pkix_issuer_id(OtpCert, self),
		{IssuerId, RestPath};
	    false  ->
		case public_key:pkix_issuer_id(OtpCert, other) of
		    {ok, IssuerId} ->
			{IssuerId, [Cert | RestPath]};
		    {error, issuer_not_found} ->
			case find_issuer(OtpCert, no_candidate) of
			    {ok, IssuerId} ->
				{IssuerId, [Cert | RestPath]};
			    Other ->
				{Other, RestPath}
			end
		end
	end,
    
    case IssuerAnPath of
	{{error, issuer_not_found}, _ } ->
	    %% The root CA was not sent and can not be found, we fail if verify = true
	    not_valid(?ALERT_REC(?FATAL, ?UNKNOWN_CA), Verify, {Cert, RestPath});
	{{SerialNr, Issuer}, Path} ->
	    case ssl_manager:lookup_trusted_cert(CertDbRef, 
							SerialNr, Issuer) of
		{ok, {BinCert,_}} ->
		    {BinCert, Path, []};
		_ ->
		    %%  Fail if verify = true
		    not_valid(?ALERT_REC(?FATAL, ?UNKNOWN_CA),
			     Verify,  {Cert, RestPath})
	    end
    end.


certificate_chain(undefined, _CertsDbRef) ->
    {error, no_cert};
certificate_chain(OwnCert, CertsDbRef) ->
    {ok, ErlCert} = public_key:pkix_decode_cert(OwnCert, otp),
    certificate_chain(ErlCert, OwnCert, CertsDbRef, [OwnCert]).

file_to_certificats(File) -> 
    {ok, List} = ssl_manager:cache_pem_file(File),
    [Bin || {cert, Bin, not_encrypted} <- List].


%% Validates ssl/tls specific extensions
validate_extensions([], ValidationState, UnknownExtensions, _, AccErr, _) -> 
    {UnknownExtensions, ValidationState, AccErr};

validate_extensions([#'Extension'{extnID = ?'id-ce-extKeyUsage',
				  extnValue = KeyUse,
				  critical = true} | Rest], 
		    ValidationState, UnknownExtensions, Verify, AccErr0, Role) ->
    case is_valid_extkey_usage(KeyUse, Role) of
	true ->
	    validate_extensions(Rest, ValidationState, UnknownExtensions, 
				Verify, AccErr0, Role);
	false ->
	    AccErr = 
		not_valid_extension({bad_cert, invalid_ext_key_usage}, Verify, AccErr0),
	    validate_extensions(Rest, ValidationState, UnknownExtensions, Verify, AccErr, Role)
    end;

validate_extensions([Extension | Rest],  ValidationState, UnknownExtensions, 
		    Verify, AccErr, Role) ->
    validate_extensions(Rest, ValidationState, [Extension | UnknownExtensions],
		       Verify, AccErr, Role).

is_valid_key_usage(KeyUse, Use) ->
    lists:member(Use, KeyUse).
 
    select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.

signature_type(RSA) when RSA == ?sha1WithRSAEncryption;
			 RSA == ?md5WithRSAEncryption ->
    rsa;
signature_type(?'id-dsa-with-sha1') ->
    dsa.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_chain(OtpCert, _Cert, CertsDbRef, Chain) ->    
    IssuerAndSelfSigned = 
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{public_key:pkix_issuer_id(OtpCert, self), true};
	    false  ->
		{public_key:pkix_issuer_id(OtpCert, other), false}
	end,
    
    case IssuerAndSelfSigned of 
	{_, true = SelfSigned} ->
	    certificate_chain(CertsDbRef, Chain, ignore, ignore, SelfSigned);
	{{error, issuer_not_found}, SelfSigned} ->
	    case find_issuer(OtpCert, no_candidate) of
		{ok, {SerialNr, Issuer}} ->
		    certificate_chain(CertsDbRef, Chain, 
				      SerialNr, Issuer, SelfSigned);
		_ ->
		    %% Guess the the issuer must be the root
		    %% certificate. The verification of the
		    %% cert chain will fail if guess is
		    %% incorrect.
		    {ok, lists:reverse(Chain)}
	    end;
	{{ok, {SerialNr, Issuer}}, SelfSigned} -> 
	    certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, SelfSigned)
    end.
  
certificate_chain(_CertsDbRef, Chain, _SerialNr, _Issuer, true) ->
    {ok, lists:reverse(Chain)};

certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, _SelfSigned) ->
    case ssl_manager:lookup_trusted_cert(CertsDbRef, 
						SerialNr, Issuer) of
	{ok, {IssuerCert, ErlCert}} ->
	    {ok, ErlCert} = public_key:pkix_decode_cert(IssuerCert, otp),
	    certificate_chain(ErlCert, IssuerCert, 
			      CertsDbRef, [IssuerCert | Chain]);
	_ ->
	    %% The trusted cert may be obmitted from the chain as the
	    %% counter part needs to have it anyway to be able to
	    %% verify it.  This will be the normal case for servers
	    %% that does not verify the clients and hence have not
	    %% specified the cacertfile.
	    {ok, lists:reverse(Chain)}		      
    end.

find_issuer(OtpCert, PrevCandidateKey) ->
    case ssl_manager:issuer_candidate(PrevCandidateKey) of
 	no_more_candidates ->
 	    {error, issuer_not_found};
 	{Key, {_Cert, ErlCertCandidate}} ->
	    case public_key:pkix_is_issuer(OtpCert, ErlCertCandidate) of
		true ->
		    public_key:pkix_issuer_id(ErlCertCandidate, self);
		false ->
		    find_issuer(OtpCert, Key)
	    end
    end.

not_valid(Alert, true, _) ->
    throw(Alert);
not_valid(_, false, {ErlCert, Path}) ->
    {ErlCert, Path, [{bad_cert, unknown_ca}]}.

is_valid_extkey_usage(KeyUse, client) ->
    %% Client wants to verify server
    is_valid_key_usage(KeyUse,?'id-kp-serverAuth');
is_valid_extkey_usage(KeyUse, server) ->
    %% Server wants to verify client
    is_valid_key_usage(KeyUse, ?'id-kp-clientAuth').

not_valid_extension(Error, true, _) ->
    throw(Error);
not_valid_extension(Error, false, AccErrors) ->
    [Error | AccErrors].
