%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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

-module(pubkey_cert).

-include("public_key.hrl").

-export([init_validation_state/3, prepare_for_next_cert/2,
 	 validate_time/3, validate_signature/6,
 	 validate_issuer/4, validate_names/6,
	 validate_revoked_status/3, validate_extensions/4,
	 normalize_general_name/1, digest_type/1, is_self_signed/1,
	 is_issuer/2, issuer_id/2, is_fixed_dh_cert/1,
	 verify_data/1, verify_fun/4]).

-define(NULL, 0).
 
%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec verify_data(DER::binary()) -> {md5 | sha,  binary(), binary()}.
%%
%% Description: Extracts data from DerCert needed to call public_key:verify/4.
%%--------------------------------------------------------------------	 
verify_data(DerCert) ->
    {ok, OtpCert} = pubkey_cert_records:decode_cert(DerCert),
    extract_verify_data(OtpCert, DerCert).

%%--------------------------------------------------------------------
-spec init_validation_state(#'OTPCertificate'{}, integer(), list()) ->
				   #path_validation_state{}.
%%
%% Description: Creates inital version of path_validation_state for
%% basic path validation of x509 certificates.
%%--------------------------------------------------------------------	 
init_validation_state(#'OTPCertificate'{} = OtpCert, DefaultPathLen, 
		      Options) ->
    PolicyTree = #policy_tree_node{valid_policy = ?anyPolicy,
				   qualifier_set = [],
				   criticality_indicator = false,
				   expected_policy_set = [?anyPolicy]},
    MaxLen =  proplists:get_value(max_path_length, Options, DefaultPathLen),
    ExplicitPolicy = policy_indicator(MaxLen,
		       proplists:get_value(explicit_policy, Options, false)),
    InhibitAnyPolicy = policy_indicator(MaxLen,
			 proplists:get_value(inhibit_any_policy, 
					     Options, false)),
    PolicyMapping = policy_indicator(MaxLen,
		      proplists:get_value(policy_mapping, Options, false)),
    {VerifyFun, UserState} =  proplists:get_value(verify_fun, Options, ?DEFAULT_VERIFYFUN),
    State = #path_validation_state{max_path_length    =  MaxLen,
				   valid_policy_tree  =  PolicyTree,
				   explicit_policy    =  ExplicitPolicy,
				   inhibit_any_policy =  InhibitAnyPolicy,
				   policy_mapping     =  PolicyMapping,
				   verify_fun         =  VerifyFun,
				   user_state	      =  UserState,
				   cert_num =           0},
    prepare_for_next_cert(OtpCert, State).

%%--------------------------------------------------------------------
-spec prepare_for_next_cert(#'OTPCertificate'{}, #path_validation_state{}) ->
				   #path_validation_state{}.
%%
%% Description: Update path_validation_state for next iteration.
%%--------------------------------------------------------------------	
prepare_for_next_cert(OtpCert, ValidationState = #path_validation_state{
				 working_public_key_algorithm = PrevAlgo,
				 working_public_key_parameters = 
				 PrevParams}) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate, 
    Issuer =  TBSCert#'OTPTBSCertificate'.subject,
   
    {Algorithm, PublicKey, PublicKeyParams0} =
	public_key_info(TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
			ValidationState),
    PublicKeyParams = 
	case PublicKeyParams0 of
	    'NULL' when Algorithm =:= PrevAlgo ->
		PrevParams;
	    asn1_NOVALUE when Algorithm =:= PrevAlgo ->
		PrevParams;
	    _ -> PublicKeyParams0
	end,
    
    ValidationState#path_validation_state{
      working_public_key_algorithm = Algorithm,
      working_public_key = PublicKey,
      working_public_key_parameters = PublicKeyParams,
      working_issuer_name = Issuer,
      cert_num = ValidationState#path_validation_state.cert_num + 1
     }.

 %%--------------------------------------------------------------------
-spec validate_time(#'OTPCertificate'{}, term(), fun()) -> term().
%%
%% Description: Check that the certificate validity period includes the 
%% current time.
%%--------------------------------------------------------------------	  
validate_time(OtpCert, UserState, VerifyFun) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    {'Validity', NotBeforeStr, NotAfterStr} 
	= TBSCert#'OTPTBSCertificate'.validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    NotBefore = time_str_2_gregorian_sec(NotBeforeStr),
    NotAfter = time_str_2_gregorian_sec(NotAfterStr),

    case ((NotBefore =< Now) and (Now =< NotAfter)) of
	true ->
	    UserState;
	false ->
	    verify_fun(OtpCert, {bad_cert, cert_expired}, UserState, VerifyFun)
    end.
%%--------------------------------------------------------------------
-spec validate_issuer(#'OTPCertificate'{}, term(), term(), fun()) -> term().
%%
%% Description: Check that the certificate issuer name is the working_issuer_name
%% in path_validation_state.
%%--------------------------------------------------------------------	
validate_issuer(OtpCert, Issuer, UserState, VerifyFun) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    case is_issuer(Issuer, TBSCert#'OTPTBSCertificate'.issuer) of
	true ->
	    UserState;
	_ ->
	    verify_fun(OtpCert, {bad_cert, invalid_issuer}, UserState, VerifyFun)
    end. 
%%--------------------------------------------------------------------
-spec validate_signature(#'OTPCertificate'{}, DER::binary(),
			 term(),term(), term(), fun()) -> term().
				
%%
%% Description: Check that the signature on the certificate can be verified using
%% working_public_key_algorithm, the working_public_key, and
%% the working_public_key_parameters in path_validation_state.
%%--------------------------------------------------------------------	
validate_signature(OtpCert, DerCert, Key, KeyParams,
		   UserState, VerifyFun) ->
    
    case verify_signature(OtpCert, DerCert, Key, KeyParams) of
	true ->
	    UserState;
	false ->
	    verify_fun(OtpCert, {bad_cert, invalid_signature}, UserState, VerifyFun)
    end.
%%--------------------------------------------------------------------
-spec validate_names(#'OTPCertificate'{}, no_constraints | list(), list(),
		     term(), term(), fun())-> term().
%%
%% Description: Validate Subject Alternative Name.
%%--------------------------------------------------------------------	
validate_names(OtpCert, Permit, Exclude, Last, UserState, VerifyFun) ->
    case is_self_signed(OtpCert) andalso (not Last) of
	true -> 
	    UserState;
	false ->
	    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate, 
	    Subject = TBSCert#'OTPTBSCertificate'.subject,
	    Extensions = 
		extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
	    AltSubject = 
		select_extension(?'id-ce-subjectAltName', Extensions),
	    
	    EmailAddress = extract_email(Subject),
	    Name = [{directoryName, Subject}|EmailAddress],
	    
	    AltNames = case AltSubject of
			   undefined -> 
			       [];
			   _ ->	
			       AltSubject#'Extension'.extnValue
		       end,
	    
	    case (is_permitted(Name, Permit) andalso 
		  is_permitted(AltNames, Permit) andalso
		  (not is_excluded(Name, Exclude)) andalso
		  (not is_excluded(AltNames, Exclude))) of
		true ->
		    UserState;
		false ->
		    verify_fun(OtpCert, {bad_cert, name_not_permitted},
			      UserState, VerifyFun)
	    end
    end.

%%--------------------------------------------------------------------
-spec validate_revoked_status(#'OTPCertificate'{}, term(), fun()) ->
				      term().
%%
%% Description: Check if certificate has been revoked.
%%--------------------------------------------------------------------	
validate_revoked_status(_OtpCert, UserState, _VerifyFun) ->
    %% TODO: Implement or leave for application?!
    %% valid |
    %% throw({bad_cert, cert_revoked})
    UserState.
%%--------------------------------------------------------------------
-spec validate_extensions(#'OTPCertificate'{}, #path_validation_state{},
			  term(), fun())->
				 {#path_validation_state{}, UserState :: term()}.
%%
%% Description: Check extensions included in basic path validation.
%%--------------------------------------------------------------------	
validate_extensions(OtpCert, ValidationState, UserState, VerifyFun) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    case TBSCert#'OTPTBSCertificate'.version of
	N when N >= 3 ->
	    Extensions = TBSCert#'OTPTBSCertificate'.extensions,
	    validate_extensions(OtpCert, Extensions,
				ValidationState, no_basic_constraint,
				is_self_signed(OtpCert), UserState, VerifyFun);
	_ -> %% Extensions not present in versions 1 & 2
	    {ValidationState, UserState}
    end.
%%--------------------------------------------------------------------
-spec normalize_general_name({rdnSequence, term()}) -> {rdnSequence, term()}. 
%%
%% Description: Normalizes a general name so that it can be easily
%%              compared to another genral name. 
%%--------------------------------------------------------------------	
normalize_general_name({rdnSequence, Issuer}) ->
    NormIssuer = do_normalize_general_name(Issuer),
    {rdnSequence, NormIssuer}.

%%--------------------------------------------------------------------
-spec is_self_signed(#'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if the certificate is self signed.
%%--------------------------------------------------------------------	
is_self_signed(#'OTPCertificate'{tbsCertificate=
				 #'OTPTBSCertificate'{issuer = Issuer, 
						      subject = Subject}}) ->
    is_issuer(Issuer, Subject).
%%--------------------------------------------------------------------
-spec is_issuer({rdnSequence, term()}, {rdnSequence, term()}) -> boolean().		       
%%
%% Description:  Checks if <Issuer> issued <Candidate>.
%%--------------------------------------------------------------------	
is_issuer({rdnSequence, Issuer}, {rdnSequence, Candidate}) ->
    is_dir_name(Issuer, Candidate, true).
%%--------------------------------------------------------------------
-spec issuer_id(#'OTPCertificate'{}, self | other) -> 
		       {ok, {integer(), term()}}  | {error, issuer_not_found}.
%%
%% Description: Extracts the issuer id from a certificate if possible.
%%--------------------------------------------------------------------	
issuer_id(Otpcert, other) ->
    TBSCert = Otpcert#'OTPCertificate'.tbsCertificate,
    Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    case select_extension(?'id-ce-authorityKeyIdentifier', Extensions) of
	undefined ->
	    {error, issuer_not_found};
	AuthKeyExt ->
	    cert_auth_key_id(AuthKeyExt#'Extension'.extnValue)
    end;
	
issuer_id(Otpcert, self) ->
    TBSCert = Otpcert#'OTPCertificate'.tbsCertificate, 
    Issuer = TBSCert#'OTPTBSCertificate'.issuer,
    SerialNr = TBSCert#'OTPTBSCertificate'.serialNumber,
    {ok, {SerialNr, normalize_general_name(Issuer)}}.  

%%--------------------------------------------------------------------
-spec is_fixed_dh_cert(#'OTPCertificate'{}) -> boolean().  
%%
%% Description: Checks if the certificate can be be used
%% for DH key agreement.
%%--------------------------------------------------------------------	
is_fixed_dh_cert(#'OTPCertificate'{tbsCertificate =
				   #'OTPTBSCertificate'{subjectPublicKeyInfo = 
							SubjectPublicKeyInfo,
							extensions = 
							Extensions}}) ->
    is_fixed_dh_cert(SubjectPublicKeyInfo, extensions_list(Extensions)). 


%%--------------------------------------------------------------------
-spec verify_fun(#'OTPCertificate'{}, {bad_cert, atom()} | {extension, #'Extension'{}}|
		 valid | valid_peer, term(), fun()) -> term().
%%
%% Description: Gives the user application the opportunity handle path
%% validation errors and unknown extensions and optional do other
%% things with a validated certificate.
%% --------------------------------------------------------------------
verify_fun(Otpcert, Result, UserState0, VerifyFun) ->
    case VerifyFun(Otpcert, Result, UserState0) of
	{valid,UserState} ->
	    UserState;
	{fail, Reason} ->
	    case Result of
		{bad_cert, _} ->
		    throw(Result);
		_ ->
		    throw({bad_cert, Reason})
	    end;
	{unknown, UserState} ->
	    case Result of
		{extension, #'Extension'{critical = true}} ->
		    throw({bad_cert, unknown_critical_extension});
		_ ->
		    UserState
	    end
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_normalize_general_name(Issuer) ->
    Normalize = fun([{Description, Type, {printableString, Value}}]) ->
			NewValue = string:to_lower(strip_spaces(Value)),
			[{Description, Type, {printableString, NewValue}}];
		   (Atter)  ->
			Atter
		end,
    lists:sort(lists:map(Normalize, Issuer)).

%% See rfc3280 4.1.2.6 Subject: regarding emails.
extract_email({rdnSequence, List}) ->
    extract_email2(List).
extract_email2([[#'AttributeTypeAndValue'{type=?'id-emailAddress', 
					  value=Mail}]|_]) ->
    [{rfc822Name, Mail}];
extract_email2([_|Rest]) ->
    extract_email2(Rest);
extract_email2([]) -> [].

extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.


extract_verify_data(OtpCert, DerCert) ->
    {0, Signature} = OtpCert#'OTPCertificate'.signature,
    SigAlgRec = OtpCert#'OTPCertificate'.signatureAlgorithm,
    SigAlg = SigAlgRec#'SignatureAlgorithm'.algorithm,
    PlainText = encoded_tbs_cert(DerCert),
    DigestType = digest_type(SigAlg),
    {DigestType, PlainText, Signature}.

verify_signature(OtpCert, DerCert, Key, KeyParams) ->
    {DigestType, PlainText, Signature} = extract_verify_data(OtpCert, DerCert),
    case Key of
	#'RSAPublicKey'{} ->
	    public_key:verify(PlainText, DigestType, Signature, Key);
	_ ->
	    public_key:verify(PlainText, DigestType, Signature, {Key, KeyParams})
    end.

encoded_tbs_cert(Cert) ->
    {ok, PKIXCert} = 
	'OTP-PUB-KEY':decode_TBSCert_exclusive(Cert),
    {'Certificate',
     {'Certificate_tbsCertificate', EncodedTBSCert}, _, _} = PKIXCert,
    EncodedTBSCert.

digest_type(?sha1WithRSAEncryption) ->
    sha;
digest_type(?md5WithRSAEncryption) ->
    md5;
digest_type(?'id-dsa-with-sha1') ->
    sha.

public_key_info(PublicKeyInfo, 
		#path_validation_state{working_public_key_algorithm =
				       WorkingAlgorithm,
				       working_public_key_parameters =
				       WorkingParams}) ->
    PublicKey = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    
    PublicKeyParams = AlgInfo#'PublicKeyAlgorithm'.parameters,
    Algorithm = AlgInfo#'PublicKeyAlgorithm'.algorithm, 
    
    NewPublicKeyParams =
	case PublicKeyParams of
	    {null, 'NULL'} when WorkingAlgorithm == Algorithm ->
		WorkingParams;
	    {params, Params} ->
		Params;
	    Params ->
		Params
	end,
    {Algorithm, PublicKey, NewPublicKeyParams}.

time_str_2_gregorian_sec({utcTime, [Y1,Y2,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,Z]}) ->
    case list_to_integer([Y1,Y2]) of
	N when N >= 50 ->
	    time_str_2_gregorian_sec({generalTime, 
				      [$1,$9,Y1,Y2,M1,M2,D1,D2,
				       H1,H2,M3,M4,S1,S2,Z]});
	_ ->
	    time_str_2_gregorian_sec({generalTime, 
				      [$2,$0,Y1,Y2,M1,M2,D1,D2,
				       H1,H2,M3,M4,S1,S2,Z]}) 
    end;

time_str_2_gregorian_sec({_,[Y1,Y2,Y3,Y4,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,$Z]}) ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
					    {Hour, Min, Sec}}).

is_dir_name([], [], _Exact) ->    true;
is_dir_name([H|R1],[H|R2], Exact) -> is_dir_name(R1,R2, Exact);
is_dir_name([[{'AttributeTypeAndValue', Type, What1}]|Rest1],
	    [[{'AttributeTypeAndValue', Type, What2}]|Rest2],Exact) ->
    case is_dir_name2(What1,What2) of
	true -> is_dir_name(Rest1,Rest2,Exact);
	false -> false
    end;
is_dir_name(_,[],false) ->
    true;
is_dir_name(_,_,_) ->
    false.

is_dir_name2(Value, Value) -> true;
is_dir_name2({printableString, Value1}, {printableString, Value2}) ->
    string:to_lower(strip_spaces(Value1)) =:= 
	string:to_lower(strip_spaces(Value2));
is_dir_name2({utf8String, Value1}, String) ->  %% BUGBUG FIX UTF8 conv
    is_dir_name2({printableString, binary_to_list(Value1)}, String);
is_dir_name2(String, {utf8String, Value1}) ->  %% BUGBUG FIX UTF8 conv
    is_dir_name2(String, {printableString, binary_to_list(Value1)});
is_dir_name2(_, _) ->
    false.

cert_auth_key_id(#'AuthorityKeyIdentifier'{authorityCertIssuer 
					   = asn1_NOVALUE}) ->
    {error, issuer_not_found};
cert_auth_key_id(#'AuthorityKeyIdentifier'{authorityCertIssuer = 
					   AuthCertIssuer,
					   authorityCertSerialNumber = 
					   SerialNr}) ->
    {ok, {SerialNr, decode_general_name(AuthCertIssuer)}}.

decode_general_name([{directoryName, Issuer}]) ->
    normalize_general_name(Issuer).

%% Strip all leading and trailing spaces and make
%% sure there is no double spaces in between. 
strip_spaces(String) ->   
    NewString = 
	lists:foldl(fun(Char, Acc) -> Acc ++ Char ++ " " end, [], 
		    string:tokens(String, " ")),
    string:strip(NewString).

select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

%% No extensions present
validate_extensions(OtpCert, asn1_NOVALUE, ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    validate_extensions(OtpCert, [], ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(_,[], ValidationState, basic_constraint, _SelfSigned,
		    UserState, _) ->
    {ValidationState, UserState};
validate_extensions(OtpCert, [], ValidationState =
			#path_validation_state{max_path_length = Len,
					       last_cert = Last},
		    no_basic_constraint, SelfSigned, UserState0, VerifyFun) ->
    case Last of
	true when SelfSigned ->
	    {ValidationState, UserState0};
	true  ->
	    {ValidationState#path_validation_state{max_path_length = Len - 1},
	     UserState0};
	%% basic_constraint must appear in certs used for digital sign
	%% see 4.2.1.10 in rfc 3280
	false ->
	    UserState = verify_fun(OtpCert, {bad_cert, missing_basic_constraint},
				   UserState0, VerifyFun),
	    case SelfSigned of
		true ->
		    {ValidationState, UserState};
		false ->
		    {ValidationState#path_validation_state{max_path_length = 
							       Len - 1},
		     UserState}
	    end
    end;

validate_extensions(OtpCert,
		    [#'Extension'{extnID = ?'id-ce-basicConstraints',
				  extnValue = 
				      #'BasicConstraints'{cA = true,
							  pathLenConstraint = N}} |
		     Rest],
		    ValidationState =
			#path_validation_state{max_path_length = Len}, _,
		    SelfSigned, UserState, VerifyFun) ->
    Length = if SelfSigned -> erlang:min(N, Len);
		true -> erlang:min(N, Len-1)
	     end,
    validate_extensions(OtpCert, Rest,
			ValidationState#path_validation_state{max_path_length =
								  Length},
			basic_constraint, SelfSigned,
			UserState, VerifyFun);
%% The pathLenConstraint field is meaningful only if cA is set to
%% TRUE.
validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-basicConstraints',
					   extnValue =
					       #'BasicConstraints'{cA = false}} |
			      Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-keyUsage',
					   extnValue = KeyUse
					  } | Rest],
		    #path_validation_state{last_cert=Last} = ValidationState,
		    ExistBasicCon, SelfSigned,
		    UserState0, VerifyFun) ->
    case Last orelse is_valid_key_usage(KeyUse, keyCertSign) of
	true ->
	    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState0, VerifyFun);
	false ->
	    UserState = verify_fun(OtpCert, {bad_cert, invalid_key_usage},
				   UserState0, VerifyFun),
	    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState, VerifyFun)
    end;

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-subjectAltName',
					   extnValue = Names,
					   critical = true} = Ext | Rest],
		    ValidationState, ExistBasicCon,
		    SelfSigned, UserState0, VerifyFun)  ->
    case validate_subject_alt_names(Names) of
	true  ->
	    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState0, VerifyFun);
	false ->
	    UserState = verify_fun(OtpCert, {extension, Ext},
				   UserState0, VerifyFun),
	    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState, VerifyFun)
    end;

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-nameConstraints',
				  extnValue = NameConst} | Rest], 
		    ValidationState, 
		    ExistBasicCon, SelfSigned, UserState, VerifyFun) ->
    Permitted = NameConst#'NameConstraints'.permittedSubtrees, 
    Excluded = NameConst#'NameConstraints'.excludedSubtrees,
    
    NewValidationState = add_name_constraints(Permitted, Excluded, 
					      ValidationState),
    
    validate_extensions(OtpCert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-certificatePolicies',
					   critical = true} = Ext| Rest], ValidationState,
		    ExistBasicCon, SelfSigned, UserState0, VerifyFun) ->
    %% TODO: Remove this clause when policy handling is
    %% fully implemented
    UserState = verify_fun(OtpCert, {extension, Ext},
			   UserState0, VerifyFun),
    validate_extensions(OtpCert,Rest, ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-certificatePolicies',
					   extnValue = #'PolicyInformation'{
					     policyIdentifier = Id,
					     policyQualifiers = Qualifier}}
			      | Rest], #path_validation_state{valid_policy_tree = Tree}
		    = ValidationState,
		    ExistBasicCon, SelfSigned, UserState, VerifyFun) ->

    %% TODO: Policy imp incomplete
    NewTree = process_policy_tree(Id, Qualifier, Tree),
    
    validate_extensions(OtpCert, Rest,
			ValidationState#path_validation_state{
			  valid_policy_tree = NewTree}, 
			ExistBasicCon, SelfSigned, UserState, VerifyFun);

validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-policyConstraints',
					   critical = true} = Ext | Rest], ValidationState,
		    ExistBasicCon, SelfSigned, UserState0, VerifyFun) ->
    %% TODO: Remove this clause when policy handling is
    %% fully implemented
    UserState = verify_fun(OtpCert, {extension, Ext},
			   UserState0, VerifyFun),
    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);
validate_extensions(OtpCert, [#'Extension'{extnID = ?'id-ce-policyConstraints',
					   extnValue = #'PolicyConstraints'{
					     requireExplicitPolicy = ExpPolicy,
					     inhibitPolicyMapping = MapPolicy}}
			      | Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    
    %% TODO: Policy imp incomplete
    NewValidationState = add_policy_constraints(ExpPolicy, MapPolicy,
						ValidationState),

    validate_extensions(OtpCert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(OtpCert, [#'Extension'{} = Extension | Rest],
		    ValidationState, ExistBasicCon,
		    SelfSigned, UserState0, VerifyFun) ->
    UserState = verify_fun(OtpCert, {extension, Extension}, UserState0, VerifyFun),
    validate_extensions(OtpCert, Rest, ValidationState, ExistBasicCon, SelfSigned,
			UserState, VerifyFun).

is_valid_key_usage(KeyUse, Use) ->
    lists:member(Use, KeyUse).
 
validate_subject_alt_names([]) ->
    false;
validate_subject_alt_names([AltName | Rest]) ->
    case is_valid_subject_alt_name(AltName) of
	true ->
	    true;
	false ->
	    validate_subject_alt_names(Rest)
    end.

is_valid_subject_alt_name({Name, Value}) when Name == rfc822Name;
					      Name == dNSName ->    
    case Value of
	"" ->
	    false;
	_  ->
	    true	   
    end;

is_valid_subject_alt_name({iPAdress, Addr}) ->
    case length(Addr) of
        4 ->  %ipv4
	    true;
	16 -> %ipv6
	    true;
	_ ->
	    false
    end;
is_valid_subject_alt_name({uniformResourceIdentifier, URI}) ->
    is_valid_uri(URI);

is_valid_subject_alt_name({directoryName, _}) ->
    true;
is_valid_subject_alt_name({_, [_|_]}) ->
    true;
is_valid_subject_alt_name({otherName, #'AnotherName'{}}) ->
    false;
is_valid_subject_alt_name({_, _}) ->
    false.

is_ip_address(Address) ->
    case inet_parse:address(Address) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

is_fully_qualified_name(_Name) ->
    true.

is_valid_uri(AbsURI) -> 
    case split_uri(AbsURI) of
	incomplete ->
	    false;
	{StrScheme, _, Host, _, _} ->
	    case string:to_lower(StrScheme) of
		Scheme when Scheme =:= "http"; Scheme =:= "ftp" ->
		    is_valid_host(Host);
		_ ->
		    false
	    end
    end.

is_valid_host(Host) ->
    case is_ip_address(Host) of
	true ->
	    true;   
	false -> 
	    is_fully_qualified_name(Host)
    end.

%% Could have a more general split URI in stdlib? Maybe when
%% regexs are improved. Needed also in inets!
split_uri(Uri) ->
    case split_uri(Uri, ":", {error, no_scheme}, 1, 1) of
	{error, no_scheme} ->
	    incomplete;
	{StrScheme, "//" ++ URIPart} ->
	    {Authority, PathQuery} = 
		split_auth_path(URIPart),
	    {UserInfo, HostPort} = 
		split_uri(Authority, "@", {"", Authority}, 1, 1),
	    {Host, Port} = 
		split_uri(HostPort, ":", {HostPort, dummy_port}, 1, 1),
	    {StrScheme, UserInfo, Host, Port, PathQuery}
    end.

split_auth_path(URIPart) ->
    case split_uri(URIPart, "/", URIPart, 1, 0) of
	Split = {_, _} ->
	    Split;
	URIPart ->
	    case split_uri(URIPart, "\\?", URIPart, 1, 0) of
		Split = {_, _} ->
		    Split;
		URIPart ->
		    {URIPart,""}
	    end
    end.

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case re:run(UriPart, SplitChar) of
	{match,[{Start, _}]} ->
	    StrPos = Start + 1,
	    {string:substr(UriPart, 1, StrPos - SkipLeft),
	     string:substr(UriPart, StrPos + SkipRight, length(UriPart))}; 
	nomatch ->
	    NoMatchResult
    end.

is_rdnSeq({rdnSequence,[]}, {rdnSequence,[none]}) -> 
    true;
is_rdnSeq({rdnSequence,DirName}, {rdnSequence,Permitted}) ->
    is_dir_name(DirName, Permitted, false).

is_permitted(_, no_constraints) ->
    true;
is_permitted(Names, Constraints) ->
    is_valid_name(Names, Constraints, true).

is_excluded([], _) ->
    false;
is_excluded(Names, Constraints) ->
    is_valid_name(Names, Constraints, false).

is_valid_name([], _, Default) ->
    Default;
is_valid_name([{Type, Name} | Rest], Constraints, Default) ->
    case type_subtree_names(Type, Constraints) of
	[_|_] = ConstraintNames ->
	    case match_name(Type, Name, ConstraintNames) of
		Default ->
		    is_valid_name(Rest, Constraints, Default);
		Fail ->
		    Fail
	    end;
	[] ->
	    is_valid_name(Rest, Constraints,Default)
    end.

add_name_constraints(NewPermittedTrees, NewExcludedTrees, 
		     #path_validation_state{
					  permitted_subtrees = PermittedTrees,
					  excluded_subtrees = ExcludedTrees} =
		     ValidationState) ->
    NewPermitted = subtree_intersection(NewPermittedTrees, PermittedTrees),
    NewExcluded = subtree_union(NewExcludedTrees, ExcludedTrees),   
    ValidationState#path_validation_state{permitted_subtrees = NewPermitted,
					  excluded_subtrees = NewExcluded}.
subtree_union(asn1_NOVALUE, Trees) ->
    Trees;
subtree_union(Trees1, Trees2) ->
    Trees1 ++ Trees2.

subtree_intersection(asn1_NOVALUE, Trees) ->
    Trees;
subtree_intersection(List, no_constraints) ->
    List;
subtree_intersection([Tree | Trees1], Trees2) ->
    Trees = is_in_intersection(Tree, Trees2),
    subtree_intersection(Trees1, Trees);
subtree_intersection([], TreesInt) ->
    TreesInt.

is_in_intersection(#'GeneralSubtree'{base  = 
				     {directoryName, {rdnSequence, Name1}}} 
		   = Name, 
		   [#'GeneralSubtree'{base = 
				      {directoryName, {rdnSequence, Name2}}} 
		    | Trees]) ->
    case is_dir_name(Name1, Name2, false) of
	true ->
	    [Name|Trees];
	false ->
	    [Name#'GeneralSubtree'{base = 
				   {directoryName, {rdnSequence,[none]}}} 
	     | Trees]
    end;
is_in_intersection(#'GeneralSubtree'{base = {ipAdress, Ip}}, 
		   Trees = [#'GeneralSubtree'{base = {ipAdress, Ip}} | _]) ->
    %% BUGBUG
    Trees;
is_in_intersection(#'GeneralSubtree'{base = {x400Address, OrAddr1}} = Addr, 
		   [#'GeneralSubtree'{base = {x400Address, OrAddr2}} 
		    | Trees]) ->	
    case is_or_address(OrAddr1, OrAddr2) of
	true ->
	    [Addr|Trees];
	false ->
	    [#'GeneralSubtree'{base = {x400Address, ""}} | Trees]
    end;

is_in_intersection(#'GeneralSubtree'{base = {Type, Name1}} = Name, 
		   [#'GeneralSubtree'{base = {Type, Name2}} 
		    | Trees]) ->
    case case_insensitive_match(Name1, Name2) of
	true ->
	    [Name|Trees];
	false ->
	    [#'GeneralSubtree'{base = {Type, ""}} | Trees]
    end;
is_in_intersection(New, []) ->
    [New];
is_in_intersection(Name, [Other | IntCandidates]) ->
    [Other|is_in_intersection(Name, IntCandidates)].

type_subtree_names(Type, SubTrees) ->
    [Name || #'GeneralSubtree'{base = {TreeType, Name}} <- SubTrees,
	     TreeType =:= Type].

match_name(rfc822Name, Name, [PermittedName | Rest]) ->
    match_name(fun is_valid_host_or_domain/2, Name, PermittedName, Rest);
	
match_name(directoryName, DirName,  [PermittedName | Rest]) ->
    match_name(fun is_rdnSeq/2, DirName, PermittedName, Rest);

match_name(uniformResourceIdentifier, URI,  [PermittedName | Rest]) ->
    case split_uri(URI) of
	incomplete ->
	    false;
	{_, _, Host, _, _} ->
	    match_name(fun is_valid_host_or_domain/2, Host, 
		       PermittedName, Rest)
    end;

match_name(emailAddress, Name, [PermittedName | Rest]) ->
    Fun = fun(Email, PermittedEmail) -> 
		  is_valid_email_address(Email, PermittedEmail,
				   string:tokens(PermittedEmail,"@"))
	  end,
     match_name(Fun, Name, PermittedName, Rest);

match_name(dNSName, Name, [PermittedName | Rest]) ->
    Fun = fun(Domain, [$.|Domain]) -> true;
	     (Name1,Name2) ->
		  lists:suffix(string:to_lower(Name2), 
			       string:to_lower(Name1))
	  end,
    match_name(Fun, Name, [$.|PermittedName], Rest);	  
	     
match_name(x400Address, OrAddress, [PermittedAddr | Rest]) ->
    match_name(fun is_or_address/2, OrAddress, PermittedAddr, Rest);

match_name(ipAdress, IP, [PermittedIP | Rest]) ->
    Fun = fun([IP1, IP2, IP3, IP4],
	      [IP5, IP6, IP7, IP8, M1, M2, M3, M4]) ->
		  is_permitted_ip([IP1, IP2, IP3, IP4],
				  [IP5, IP6, IP7, IP8],
				  [M1, M2, M3, M4]);
	     ([IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,
	       IP9, IP10, IP11, IP12, IP13, IP14, IP15, IP16],
	      [IP17, IP18, IP19, IP20, IP21, IP22, IP23, IP24,
	       IP25, IP26, IP27, IP28, IP29, IP30, IP31, IP32,
	       M1, M2, M3, M4, M5, M6, M7, M8,
	       M9, M10, M11, M12, M13, M14, M15, M16]) ->
		  is_permitted_ip([IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,
				   IP9, IP10, IP11, IP12, IP13,
				   IP14, IP15, IP16],
				  [IP17, IP18, IP19, IP20, IP21, IP22, IP23, 
				   IP24,IP25, IP26, IP27, IP28, IP29, IP30, 
				   IP31, IP32],
				    [M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, 
				     M11, M12, M13, M14, M15, M16]);
	     (_,_) ->
		  false
	  end,
    match_name(Fun, IP, PermittedIP, Rest).
    
match_name(Fun, Name, PermittedName, []) ->
    Fun(Name, PermittedName);
match_name(Fun, Name, PermittedName, [Head | Tail]) ->
    case Fun(Name, PermittedName) of
	true ->
	    true;
	false ->
	    match_name(Fun, Name, Head, Tail)
    end.

is_permitted_ip([], [], []) ->
    true;
is_permitted_ip([CandidatIp | CandidatIpRest], 
		[PermittedIp | PermittedIpRest], [Mask | MaskRest] ) -> 
    case mask_cmp(CandidatIp, PermittedIp, Mask) of
	true ->
	    is_permitted_ip(CandidatIpRest, PermittedIpRest, MaskRest);
	false ->
	    false
    end.

mask_cmp(Canditate, Permitted, Mask) ->
    (Canditate band Mask) == Permitted.

is_valid_host_or_domain(Canditate, [$.|_] = Permitted) ->
    is_suffix(Permitted, Canditate);
is_valid_host_or_domain(Canditate, Permitted) ->
    case string:tokens(Canditate,"@") of
	[CanditateHost] ->
	    case_insensitive_match(CanditateHost, Permitted);
	[_, CanditateHost] ->
	    case_insensitive_match(CanditateHost, Permitted)
    end.
is_valid_email_address(Canditate, [$.|Permitted], [_]) ->
    is_suffix(Permitted, Canditate);

is_valid_email_address(Canditate, PermittedHost, [_]) ->
    [_ , CanditateHost] = string:tokens(Canditate,"@"),
    case_insensitive_match(CanditateHost, PermittedHost);

is_valid_email_address(Canditate, Permitted, [_, _]) ->
    case_insensitive_match(Canditate, Permitted).

is_suffix(Suffix, Str) ->
    lists:suffix(string:to_lower(Suffix), string:to_lower(Str)).
case_insensitive_match(Str1, Str2) ->
    string:to_lower(Str1) == string:to_lower(Str2).

is_or_address(Address, Canditate) ->
    %% TODO: Is case_insensitive_match sufficient?
    %% study rfc2156 probably need more a complex check.
    is_double_quoted(Address) andalso 
	is_double_quoted(Canditate) andalso 
	case_insensitive_match(Address, Canditate).
    
is_double_quoted(["\"" | Tail]) ->
    is_double_quote(lists:last(Tail));
is_double_quoted("%22" ++ Tail) ->
    case lists:reverse(Tail) of
	[A, B, C | _] ->
	    is_double_quote([C, B, A]);
	_ ->
	    false
    end;

is_double_quoted(_) ->
    false.

is_double_quote("%22") ->
    true;
is_double_quote("\"") ->
    true;
is_double_quote(_) ->
    false.

add_policy_constraints(ExpPolicy, MapPolicy, 
		       #path_validation_state{cert_num = CertNum,
					      explicit_policy = CurExpPolicy,
					      policy_mapping = CurMapPolicy} = 
		       ValidationState) ->
    
    NewExpPolicy = policy_constraint(CurExpPolicy, ExpPolicy, CertNum),
    NewMapPolicy = policy_constraint(CurMapPolicy, MapPolicy, CertNum),

    ValidationState#path_validation_state{explicit_policy = NewExpPolicy,
					  policy_mapping = NewMapPolicy}.

policy_constraint(Current, asn1_NOVALUE, _) ->
    Current;
policy_constraint(Current, New, CertNum) ->
    erlang:min(Current, New + CertNum).

process_policy_tree(_,_, ?NULL) ->
    ?NULL;
process_policy_tree(_Id, _Qualifier, Tree) ->
    %% TODO real imp.
    Tree.

policy_indicator(_, true) ->
    0;
policy_indicator(N, false) ->
   N + 1.

is_fixed_dh_cert(PublicKeyInfo, Extensions) ->
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    Algorithm = AlgInfo#'PublicKeyAlgorithm'.algorithm,
   
    case select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	    is_dh(Algorithm);
	#'Extension'{extnValue=KeyUse} ->
	    is_dh(Algorithm) andalso is_valid_key_usage(KeyUse, keyAgreement)
    end.
	
is_dh(?'dhpublicnumber')->
    true;
is_dh(_) ->
    false.
