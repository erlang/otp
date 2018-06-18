%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

-module(pubkey_crl).

-include("public_key.hrl").

-export([validate/7, init_revokation_state/0, fresh_crl/3, verify_crl_signature/4,
	 is_delta_crl/1, combines/2, match_one/2]).

-record(userstate, {dpcrls,
		    idp
		   }).

validate(OtpCert, OtherDPCRLs, DP, {DerCRL, CRL}, {DerDeltaCRL, DeltaCRL},
	     Options, RevokedState0) ->
    RevokedState =
	case verify_crl(OtpCert, DP, CRL, DerCRL, DeltaCRL,
			DerDeltaCRL, OtherDPCRLs, Options, RevokedState0) of
	    {valid, Revoked, DeltaRevoked, RevokedState1, IDP} ->
		TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
		SerialNumber = TBSCert#'OTPTBSCertificate'.serialNumber,
		CertIssuer = TBSCert#'OTPTBSCertificate'.issuer,
		TBSCRL = CRL#'CertificateList'.tbsCertList,
		CRLIssuer =  TBSCRL#'TBSCertList'.issuer,
		AltNames = case pubkey_cert:select_extension(?'id-ce-subjectAltName',
							     TBSCert#'OTPTBSCertificate'.extensions) of
			       #'Extension'{extnValue = Value} ->
				   Value;
			       _ ->
				   []
			   end,
		revoked_status(DP, IDP, {directoryName, CRLIssuer},
			       [ {directoryName, CertIssuer} | AltNames], SerialNumber, Revoked,
			       DeltaRevoked, RevokedState1);
	    {invalid, Revoked} ->
		Revoked
	end,
    crl_status(RevokedState).

init_revokation_state() ->
    #revoke_state{reasons_mask = sets:new(),
		  interim_reasons_mask = sets:new(),
		  cert_status = unrevoked,
                  details = []}.

fresh_crl(_, {undefined, undefined}, _) ->
    %% Typically happens when there is no delta CRL that covers a CRL
    no_fresh_crl;

fresh_crl(DP, {_, #'CertificateList'{tbsCertList = TBSCRL}} = CRL, CallBack) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UpdateTime =
	pubkey_cert:time_str_2_gregorian_sec(TBSCRL#'TBSCertList'.nextUpdate),
    case Now >= UpdateTime of
	true ->
	    case CallBack(DP, CRL) of
		CRL ->
		    no_fresh_crl;
	       NewCRL ->
		    fresh_crl(DP, NewCRL, CallBack)
	    end;
	false ->
	    {fresh, CRL}
    end.

is_delta_crl(#'CertificateList'{tbsCertList = TBSCRL}) ->
    Extensions = TBSCRL#'TBSCertList'.crlExtensions,
    case pubkey_cert:select_extension(?'id-ce-deltaCRLIndicator',
				      Extensions) of
	undefined ->
	    false;
	_ ->
	    true
    end.

combines(CRL, DeltaCRL) ->
    check_crl_num(CRL, DeltaCRL) andalso
	check_delta_issuer_and_scope(CRL, DeltaCRL).

crl_status(State)->
    %% Fun argument is to enable future implementation of CRL checking
    %% that does not care about all possible reasons.
    crl_status(State, fun all_reasons/0).

crl_status({skip, #revoke_state{cert_status = Status} = RevokedState}, _) ->
    {undetermined, status(Status), RevokedState};

crl_status(#revoke_state{cert_status = unrevoked = Status,
			 valid_ext = false} = RevokedState, _) ->
    {undetermined, Status, RevokedState};

crl_status(#revoke_state{cert_status = Status,
			 valid_ext = false}, _) ->
    {finished, status(Status)};

crl_status(#revoke_state{reasons_mask = Mask,
			 cert_status = Status,
			 valid_ext = true} = RevokedState, Fun) ->
    case is_all_reasons(Mask, Fun) of
	true ->
	    {finished, status(Status)};
	false when (Status == unrevoked) ->
	    {undetermined, Status, RevokedState};
	_ ->
	    {finished ,status(Status)}
    end.

verify_crl(OtpCert, DP, CRL, DerCRL, DeltaCRL, DerDeltaCRL, OtherDPCRLs,
	   Options, State0) ->
    #'CertificateList'{tbsCertList =
			   #'TBSCertList'{crlExtensions = Extensions,
					  revokedCertificates = TmpRevoked}
		      } = CRL,
    Revoked = revoked(TmpRevoked),
    IDP = issuing_distribution_point(Extensions),

    DeltaRevoked = delta_revoked(DeltaCRL),

    ValidExt = verify_extensions(Extensions) and
	verify_extensions(Revoked),

    IntMask = compute_interim_reasons_mask(DP, IDP),

    RevokedState =
	State0#revoke_state{interim_reasons_mask = IntMask,
			    valid_ext = ValidExt},

    {Fun, AdditionalArgs} =  IssuerFun = proplists:get_value(issuer_fun, Options),

    try verify_issuer_and_scope(OtpCert, DP, IDP, CRL) of
	{ok, Issuer} ->
	    case Fun(DP, CRL, Issuer, AdditionalArgs) of
		{ok, TrustedOtpCert, Path} ->
		    verify_mask_and_signatures(Revoked, DeltaRevoked,
					       RevokedState,
					       CRL, DerCRL, DeltaCRL, DerDeltaCRL,
					       IssuerFun, TrustedOtpCert, Path, OtherDPCRLs, IDP);
	        _ ->
                    Details = RevokedState#revoke_state.details,
		    {invalid, RevokedState#revoke_state{valid_ext = ValidExt, details = [{{bad_crl, no_issuer_cert_chain}, CRL} | Details]}}
            end;
	{error, issuer_not_found} ->
	    case Fun(DP, CRL, issuer_not_found, AdditionalArgs) of
		{ok, TrustedOtpCert, Path} ->
		    verify_mask_and_signatures(Revoked, DeltaRevoked,
					       RevokedState, CRL, DerCRL, DeltaCRL,
					       DerDeltaCRL, IssuerFun,
					       TrustedOtpCert, Path, OtherDPCRLs, IDP);
		_ ->
                    Details = State0#revoke_state.details,
		    {invalid, {skip, State0#revoke_state{details = [{{bad_crl, no_issuer_cert_chain}, CRL} | Details] }}}
            end
    catch
	throw:{bad_crl, invalid_issuer} = Reason ->
            Details = RevokedState#revoke_state.details,
	    {invalid, {skip, RevokedState#revoke_state{details = [{Reason, CRL} | Details]}}};
	throw:Reason ->
            Details = RevokedState#revoke_state.details,
	    {invalid, RevokedState#revoke_state{details =  [{Reason, CRL} | Details]}}
    end.

verify_mask_and_signatures(Revoked, DeltaRevoked, RevokedState, CRL, DerCRL, DeltaCRL, DerDeltaCRL,
			   IssuerFun, TrustedOtpCert, Path, OtherDPCRLs, IDP) ->

    ReasonsMask = sets:union(RevokedState#revoke_state.reasons_mask,
			     RevokedState#revoke_state.interim_reasons_mask),
    try
	verify_interim_reasons_mask(RevokedState),
	true = verify_crl_signatures(CRL, DerCRL, DeltaCRL, DerDeltaCRL,
				     TrustedOtpCert, Path, IssuerFun, OtherDPCRLs, IDP),
	{valid, Revoked, DeltaRevoked, RevokedState#revoke_state{reasons_mask = ReasonsMask}, IDP}
    catch
	throw:Reason ->
            Details = RevokedState#revoke_state.details,
	    {invalid, RevokedState#revoke_state{details =  [{Reason, CRL} | Details]}};
	error:{badmatch, _} ->
            Details = RevokedState#revoke_state.details,
	    {invalid, RevokedState#revoke_state{details = [{{bad_crl, invalid_signature}, CRL} | Details]}}
    end.


verify_crl_signatures(CRL, DerCRL, DeltaCRL, DerDeltaCRL, TrustedOtpCert, Path,
		      IssuerFun, OtherDPCRLs, IDP) ->
    try
	VerifyFunAndState =
	    {fun(_, {bad_cert, _} = Reason, _UserState) ->
		    {fail, Reason};
		(_,{extension, _}, UserState) ->
		     {unknown, UserState};
		(_Cert, valid, UserState) ->
		     {valid, UserState};
		(Cert, valid_peer, UserState) ->
		     case verify_crl_keybit(Cert, cRLSign) of
			 true ->
			     handle_crlsigner(Cert, IssuerFun, UserState);
			 false ->
			     {fail, crl_sign_bit_not_set}
		     end
	     end, #userstate{dpcrls = OtherDPCRLs, idp = IDP}},

	{ok, {{_,Key, KeyParams},_}} =
	    public_key:pkix_path_validation(TrustedOtpCert, Path,
					    [{verify_fun, VerifyFunAndState}]),
	true = verify_crl_signature(CRL, DerCRL, Key, KeyParams),
	true = verify_crl_signature(DeltaCRL, DerDeltaCRL, Key, KeyParams)
    catch
	error:{badmatch, _} ->
	    false
    end.

handle_crlsigner(OtpCert, IssuerFun, #userstate{idp = IDP} = UserState) ->
    case verify_crl_keybit(OtpCert, keyCertSign) of
	true ->
	    {valid, UserState};
	false ->
	    case not is_indirect_crl(IDP) andalso not public_key:pkix_is_self_signed(OtpCert) of
		true ->
		    validate_crl_signing_cert(OtpCert, IssuerFun, UserState);
		false ->
		    {valid, UserState}
	    end
    end.

validate_crl_signing_cert(_, _,#userstate{dpcrls = []} = UserState) ->
    {valid, UserState};
validate_crl_signing_cert(OtpCert, IssuerFun, #userstate{dpcrls = CRLInfo} = UserState) ->
    case public_key:pkix_crls_validate(OtpCert, CRLInfo, [{issuer_fun, IssuerFun}]) of
	valid ->
	    {valid, UserState};
	Reason ->
	    {fail, Reason}
    end.

delta_revoked(undefined)->
    [];
delta_revoked(#'CertificateList'{tbsCertList =
				     #'TBSCertList'{revokedCertificates
						    = DeltaRevoked}}) ->
    revoked(DeltaRevoked).

revoked(asn1_NOVALUE) ->
    [];
revoked(Revoked) ->
   Revoked.

revoked_status(DP, IDP, CRLIssuer, Names, SerialNumber, Revoked, DeltaRevoked, RevokedState0) ->
    DefaultIssuer0 = default_issuer(CRLIssuer, DeltaRevoked),
    RevokedState1 = check_revoked(DP, IDP, DefaultIssuer0, Names, SerialNumber, DeltaRevoked, RevokedState0),
    RevokedState = case RevokedState1#revoke_state.cert_status of
		       unrevoked when RevokedState1#revoke_state.cert_status =/= removeFromCRL ->
			   DefaultIssuer = default_issuer(CRLIssuer, Revoked),
			   check_revoked(DP, IDP, DefaultIssuer, Names, SerialNumber,
					 Revoked, RevokedState1);
		       _ ->
			   RevokedState1
		   end,
    case RevokedState#revoke_state.cert_status of
	removeFromCRL ->
	    RevokedState#revoke_state{cert_status = unrevoked};
	_ ->
	    RevokedState
    end.

default_issuer(_, []) ->
    undefined;
default_issuer(Default, [#'TBSCertList_revokedCertificates_SEQOF'{crlEntryExtensions = Extensions}| _]) ->
    case extension_value(?'id-ce-certificateIssuer', 'GeneralNames', Extensions) of
	undefined ->
	    [pubkey_cert_records:transform(Default, decode)];
	GeneralNames ->
	    gen_names(GeneralNames)
    end.

is_all_reasons(Mask, AllReasonsFun) ->
    AllReasons = AllReasonsFun(),
    case sets:is_subset(AllReasons, Mask) of
	true ->
	    true;
	false ->
	    %% As the "uspecified" reason should not
	    %% be explicitly used according to RFC 3280
	    %% and the conformance tests have test cases
	    %% that should succed, and that does not specify
	    %% "unspecified", we tolorate that it is not included.
	    sets:is_subset(sets:del_element(unspecified, AllReasons), Mask)
    end.

all_reasons() ->
    sets:from_list([unspecified, keyCompromise,
		    cACompromise, affiliationChanged, superseded,
		    cessationOfOperation, certificateHold,
		    privilegeWithdrawn, aACompromise]).

verify_issuer_and_scope(#'OTPCertificate'{tbsCertificate = TBSCert} = Cert,
			#'DistributionPoint'{cRLIssuer = DPIssuer} = DP, IDP,
			#'CertificateList'{tbsCertList = TBSCRL} = CRL)
  when DPIssuer =/= asn1_NOVALUE ->
    CRLIssuer = pubkey_cert_records:transform(TBSCRL#'TBSCertList'.issuer, decode),
    Issuer = dp_crlissuer_to_issuer(DPIssuer),
    case pubkey_cert:is_issuer(Issuer, CRLIssuer) and is_indirect_crl(IDP) of
	true ->
	    verify_scope(Cert, DP, IDP),
	    issuer_id(Cert, CRL);
	false ->
	    %% otherwise verify that the CRL issuer matches the certificate issuer
	    verify_issuer_and_scope(Cert, DP#'DistributionPoint'{
					    distributionPoint = [TBSCert#'OTPTBSCertificate'.issuer],
					    cRLIssuer = asn1_NOVALUE},
				    IDP, CRL)
    end;
verify_issuer_and_scope(#'OTPCertificate'{tbsCertificate = TBSCert}= Cert,
			DP, IDP,
			#'CertificateList'{tbsCertList = TBSCRL}) ->
    CRLIssuer = pubkey_cert_records:transform(TBSCRL#'TBSCertList'.issuer, decode),
    CertIssuer = TBSCert#'OTPTBSCertificate'.issuer,
    case pubkey_cert:is_issuer(CertIssuer, CRLIssuer) of
	true ->
	    verify_scope(Cert, DP, IDP),
	    issuer_id(Cert);
	false ->
	    throw({bad_crl, invalid_issuer})
    end.

dp_crlissuer_to_issuer(DPCRLIssuer) ->
     [{directoryName, Issuer}] = pubkey_cert_records:transform(DPCRLIssuer, decode),
     Issuer.

is_indirect_crl(#'IssuingDistributionPoint'{indirectCRL = Value})->
    Value;
is_indirect_crl(_) ->
    false.

verify_scope(_,_, undefined) ->
    ok;
verify_scope(#'OTPCertificate'{tbsCertificate = TBSCert}, #'DistributionPoint'{cRLIssuer = DPIssuer} = DP, IDP) ->
    CertIssuer = TBSCert#'OTPTBSCertificate'.issuer,
    Names = case gen_names(DPIssuer) of
		[{directoryName, TNames}] ->
		    TNames;
		Other ->
		    Other
	    end,
    DPName =  dp_names(DP#'DistributionPoint'.distributionPoint, Names, CertIssuer),
    IDPName = dp_names(IDP#'IssuingDistributionPoint'.distributionPoint, Names, CertIssuer),
    verify_scope(DPName, IDPName, Names, TBSCert, IDP).

verify_scope(asn1_NOVALUE, _, asn1_NOVALUE, _, _) ->
    throw({bad_crl, scope_error});
verify_scope(asn1_NOVALUE, IDPName, DPIssuerNames, TBSCert, IDP) ->
    verify_dp_name(IDPName, DPIssuerNames),
    verify_dp_bools(TBSCert, IDP);

verify_scope(DPName, IDPName, _, TBSCert, IDP) ->
    verify_dp_name(IDPName, DPName),
    verify_dp_bools(TBSCert, IDP).

dp_names(asn1_NOVALUE, _, _) ->
    asn1_NOVALUE;
dp_names({fullName, Name}, _, _) ->
     gen_names(Name);
dp_names({nameRelativeToCRLIssuer, Fragment}, asn1_NOVALUE, {rdnSequence, RelativeDestinguistNames}) ->
    [{directoryName, {rdnSequence, RelativeDestinguistNames ++
			  [lists:map(fun(AttrAndValue) ->
					     pubkey_cert_records:transform(AttrAndValue, decode)
				     end, Fragment)]}}];
dp_names({nameRelativeToCRLIssuer, Fragment},{rdnSequence, RelativeDestinguistNames}, _) ->
     [{directoryName, {rdnSequence, RelativeDestinguistNames ++
			   [lists:map(fun(AttrAndValue) ->
					      pubkey_cert_records:transform(AttrAndValue, decode)
				      end, Fragment)]}}];
dp_names([{rdnSequence, _}] = Name0, _,_) ->
    [Name] = pubkey_cert_records:transform(Name0, decode),
    [{directoryName, Name}].

gen_names(asn1_NOVALUE) ->
    asn1_NOVALUE;
gen_names([]) ->
    [];
gen_names([{NameType, Name} | Rest]) ->
    [ {NameType, pubkey_cert_records:transform(Name, decode)} | gen_names(Rest)].

verify_dp_name(asn1_NOVALUE, _) ->
    ok;

verify_dp_name(IDPNames, DPorIssuerNames) ->
    case match_one(DPorIssuerNames, IDPNames) of
	true ->
	    ok;
	false ->
	    throw({bad_crl, scope_error})
    end.

match_one([], _) ->
    false;
match_one([{Type, Name} | Names], CandidateNames) ->
    Candidates = [NameName || {NameType, NameName} <- CandidateNames, 
			      NameType == Type],
    case Candidates of
	[] ->
	    false;
	[_|_] ->
	    case pubkey_cert:match_name(Type, Name, Candidates) of
		true ->
		    true;
		false ->
		    match_one(Names, CandidateNames)
	    end
    end.

verify_dp_bools(TBSCert, IDP) ->
    BasicConstraints =
	pubkey_cert:select_extension(?'id-ce-basicConstraints',
				     TBSCert#'OTPTBSCertificate'.extensions),

    case verify_onlyContainsUserCerts(BasicConstraints, IDP) andalso
	verify_onlyContainsCACerts(BasicConstraints, IDP) andalso
	verify_onlyContainsAttributeCerts(IDP) of
	true ->
	    ok;
	_  ->
	    throw({bad_crl, scope_error})
    end.

verify_onlyContainsUserCerts(
  #'Extension'{extnValue = #'BasicConstraints'{cA = true}},
  #'IssuingDistributionPoint'{onlyContainsUserCerts = true}) ->
    false;
verify_onlyContainsUserCerts(_,_) ->
    true.

verify_onlyContainsCACerts(
  #'Extension'{extnValue = #'BasicConstraints'{cA = true}},
  #'IssuingDistributionPoint'{onlyContainsCACerts = true}) ->
    true;
verify_onlyContainsCACerts(_,#'IssuingDistributionPoint'{onlyContainsCACerts = true}) ->
    false;
verify_onlyContainsCACerts(_,_) ->
    true.

verify_onlyContainsAttributeCerts(
  #'IssuingDistributionPoint'{onlyContainsAttributeCerts = Bool}) ->
    not Bool.

check_crl_num(#'CertificateList'{tbsCertList = TBSCRL},
	      #'CertificateList'{tbsCertList = TBSDeltaCRL})->
    Extensions = TBSCRL#'TBSCertList'.crlExtensions,
    DeltaExtensions = TBSDeltaCRL#'TBSCertList'.crlExtensions,

    try
	CRLNum = assert_extension_value(?'id-ce-cRLNumber', 'CRLNumber', Extensions),
	DeltaBaseNum = assert_extension_value(?'id-ce-deltaCRLIndicator',
					      'CRLNumber', DeltaExtensions),
	DeltaCRLNum = assert_extension_value(?'id-ce-cRLNumber', 'CRLNumber', DeltaExtensions),
	(CRLNum >= DeltaBaseNum) andalso (CRLNum < DeltaCRLNum)
    catch
	throw:no_extension_present ->
	    false
    end;
check_crl_num(_,_) ->
    false.


extension_value(Extension, ExtType, Extensions) ->
    case pubkey_cert:select_extension(Extension, Extensions) of
	#'Extension'{extnValue = Value} ->
	    public_key:der_decode(ExtType, iolist_to_binary(Value));
	_ ->
	    undefined
    end.


assert_extension_value(Extension, ExtType, Extensions) ->
    case extension_value(Extension, ExtType, Extensions) of
	undefined ->
	    throw(no_extension_present);
	Value ->
	    Value
    end.

check_delta_issuer_and_scope(_, undefined) ->
    true;
check_delta_issuer_and_scope(#'CertificateList'{tbsCertList = TBSCRL},
			     #'CertificateList'{tbsCertList = TBSDeltaCRL}) ->
    case pubkey_cert:is_issuer(TBSCRL#'TBSCertList'.issuer,
			       TBSDeltaCRL#'TBSCertList'.issuer) of
	true ->
	    check_delta_scope(TBSCRL, TBSDeltaCRL);
	false ->
	    false
    end.

check_delta_scope(#'TBSCertList'{crlExtensions = Extensions},
		   #'TBSCertList'{crlExtensions = DeltaExtensions})->
    IDP = issuing_distribution_point(Extensions),
    DeltaIDP = issuing_distribution_point(DeltaExtensions),

    AuthKey = authority_key_identifier(Extensions),
    DeltaAuthKey = authority_key_identifier(DeltaExtensions),
    is_match(IDP, DeltaIDP) andalso is_match(AuthKey, DeltaAuthKey).

is_match(X, X) ->
    true;
is_match(_,_) ->
    false.

compute_interim_reasons_mask(#'DistributionPoint'{reasons = asn1_NOVALUE},
			     #'IssuingDistributionPoint'{onlySomeReasons =
							     asn1_NOVALUE}) ->
    all_reasons();
compute_interim_reasons_mask(#'DistributionPoint'{reasons = asn1_NOVALUE},
			     undefined) ->
    all_reasons();

compute_interim_reasons_mask(#'DistributionPoint'{reasons = asn1_NOVALUE},
			     #'IssuingDistributionPoint'{onlySomeReasons =
							     IDPReasons}) ->
    sets:from_list(IDPReasons);
compute_interim_reasons_mask(#'DistributionPoint'{reasons = DPReasons},
			     #'IssuingDistributionPoint'{onlySomeReasons =
							     asn1_NOVALUE}) ->
    sets:from_list(DPReasons);
compute_interim_reasons_mask(#'DistributionPoint'{reasons = DPReasons},
			     undefined) ->
    sets:from_list(DPReasons);
compute_interim_reasons_mask(#'DistributionPoint'{reasons = DPReasons},
			     #'IssuingDistributionPoint'{onlySomeReasons =
							     IDPReasons}) ->
    sets:intersection(sets:from_list(DPReasons), sets:from_list(IDPReasons)).

verify_interim_reasons_mask(#revoke_state{reasons_mask = Mask,
					  interim_reasons_mask = IntMask}) ->
    case sets:fold(fun(Element, Acc) ->
			   case sets:is_element(Element, Mask) of
			       true ->
				   Acc;
			   false ->
				   true
			   end
		   end, false, IntMask) of
	true ->
	    ok;
	false ->
	    throw({bad_crl, mask_error})
    end.

verify_crl_signature(undefined, undefined, _,_) ->
    true;
verify_crl_signature(CRL, DerCRL, Key, KeyParams) ->
    {DigestType, PlainText, Signature} = extract_crl_verify_data(CRL, DerCRL),
    case Key of
	#'RSAPublicKey'{} ->
	    public_key:verify(PlainText, DigestType, Signature, Key);
	_ ->
	    public_key:verify(PlainText, DigestType, Signature,
			      {Key, KeyParams})
    end.
extract_crl_verify_data(CRL, DerCRL) ->
    Signature = CRL#'CertificateList'.signature,
    #'AlgorithmIdentifier'{algorithm = SigAlg} =
	CRL#'CertificateList'.signatureAlgorithm,
    PlainText = encoded_tbs_crl(DerCRL),
    {DigestType, _} = public_key:pkix_sign_types(SigAlg),
    {DigestType, PlainText, Signature}.

encoded_tbs_crl(CRL) ->
    {ok, PKIXCRL} =
	'OTP-PUB-KEY':decode_TBSCertList_exclusive(CRL),
    {'CertificateList',
     {'CertificateList_tbsCertList', EncodedTBSCertList}, _, _} = PKIXCRL,
    EncodedTBSCertList.

check_revoked(_,_,_,_,_,[], State) ->
    State;
check_revoked(#'DistributionPoint'{cRLIssuer = DPIssuer} = DP, IDP, DefaultIssuer0, Names, SerialNr,
	      [#'TBSCertList_revokedCertificates_SEQOF'{userCertificate =
							    SerialNr,
							crlEntryExtensions =
							    Extensions}| Rest],
	      State) ->
    Reason = revoked_reason(Extensions),
    case (DPIssuer =/= asn1_NOVALUE) and is_indirect_crl(IDP) of
	true ->
	    handle_indirect_crl_check(DP, IDP, DefaultIssuer0, Names, SerialNr, Extensions, Reason, Rest, State);
	false ->
	    State#revoke_state{cert_status = Reason}
    end;

check_revoked(DP, IDP, DefaultIssuer0, Names, SerialNr,
	      [#'TBSCertList_revokedCertificates_SEQOF'{crlEntryExtensions =
							    Extensions}| Rest], State) ->
    DefaultIssuer = case extension_value(?'id-ce-certificateIssuer', 'GeneralNames', Extensions) of
			undefined ->
			    DefaultIssuer0;
			GeneralNames ->
			    gen_names(GeneralNames)
		    end,
    check_revoked(DP, IDP, DefaultIssuer, Names, SerialNr, Rest, State).

handle_indirect_crl_check(DP, IDP, DefaultIssuer0, Names, SerialNr, Extensions, Reason, Rest, State) ->
    case check_crl_issuer_extension(Names, Extensions, DefaultIssuer0) of
	{true, _} ->
	    State#revoke_state{cert_status = Reason};
	{false, DefaultIssuer} ->
	    check_revoked(DP, IDP, DefaultIssuer, Names, SerialNr, Rest, State)
    end.

check_crl_issuer_extension(Names, Extensions, Default0) ->
    case extension_value(?'id-ce-certificateIssuer', 'GeneralNames', Extensions) of
	undefined ->
	    {match_one(Default0, Names), Default0};
	GeneralNames ->
	    Default = gen_names(GeneralNames),
	    {match_one(Default, Names), Default}
    end.

revoked_reason(Extensions) ->
    case extension_value(?'id-ce-cRLReasons', 'CRLReason', Extensions) of
	undefined ->
	    unspecified;
	Value ->
	    Value
    end.

verify_crl_keybit(#'OTPCertificate'{tbsCertificate = TBS}, Bit) ->
    case pubkey_cert:select_extension( ?'id-ce-keyUsage',
				       TBS#'OTPTBSCertificate'.extensions) of
	#'Extension'{extnID = ?'id-ce-keyUsage',
		     extnValue = KeyUse} ->
	    lists:member(Bit, KeyUse);
	_ ->
	    true
    end.

issuer_id(Cert, #'CertificateList'{tbsCertList = TBSCRL}) ->
    Extensions =
	pubkey_cert:extensions_list(TBSCRL#'TBSCertList'.crlExtensions),
    case authority_key_identifier(Extensions) of
	undefined ->
	    issuer_id(Cert);
	#'AuthorityKeyIdentifier'{authorityCertIssuer = asn1_NOVALUE,
				  authorityCertSerialNumber = asn1_NOVALUE} ->
	    issuer_id(Cert);
	#'AuthorityKeyIdentifier'{authorityCertIssuer = Issuer,
				  authorityCertSerialNumber = Nr}  ->
	    {ok, {Nr, Issuer}}
    end.

issuer_id(#'OTPCertificate'{} = Cert) ->
    case public_key:pkix_is_self_signed(Cert) of
	true ->
	    public_key:pkix_issuer_id(Cert, self);
	false ->
	    public_key:pkix_issuer_id(Cert, other)
    end.

status(unrevoked) ->
    unrevoked;
status(Reason) ->
    {revoked, Reason}.

verify_extensions([#'TBSCertList_revokedCertificates_SEQOF'{crlEntryExtensions = Ext} | Rest]) ->
    verify_extensions(pubkey_cert:extensions_list(Ext)) and verify_extensions(Rest);
verify_extensions([]) ->
    true;
verify_extensions(asn1_NOVALUE) ->
    true;
verify_extensions([#'Extension'{critical = true, extnID = Id} | Rest]) ->
    case lists:member(Id, [?'id-ce-authorityKeyIdentifier',
			   ?'id-ce-issuerAltName',
			   ?'id-ce-cRLNumber',
			   ?'id-ce-certificateIssuer',
			   ?'id-ce-deltaCRLIndicator',
			   ?'id-ce-issuingDistributionPoint',
			   ?'id-ce-freshestCRL']) of
	true ->
	    verify_extensions(Rest);
	false ->
	    false
    end;
verify_extensions([_Ext | Rest]) ->
    verify_extensions(Rest).

issuing_distribution_point(Extensions) ->
    Enc = extension_value(?'id-ce-issuingDistributionPoint',
			  'IssuingDistributionPoint', Extensions),
    pubkey_cert_records:transform(Enc, decode).

authority_key_identifier(Extensions) ->
    Enc = extension_value(?'id-ce-authorityKeyIdentifier',
			  'AuthorityKeyIdentifier', Extensions),
    pubkey_cert_records:transform(Enc, decode).
