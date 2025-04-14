%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(pubkey_cert).
-moduledoc false.

%% path validation
-export([init_validation_state/3,
         validate_extensions/4,
         validate_time/3,
         validate_issuer/4,
         validate_names/6,
         validate_signature/6,
         verify_data/1,
         verify_fun/4,
         prepare_for_next_cert/2,
         apply_fun/5]).

%% Utility functions
-export([normalize_general_name/1,
         is_self_signed/1,
	 is_issuer/2,
         issuer_id/2,
         subject_id/1,
         distribution_points/1,
	 is_fixed_dh_cert/1,
         select_extension/2,
         match_name/3,
	 extensions_list/1,
         cert_auth_key_id/1,
         time_str_2_gregorian_sec/1
        ]).

%% Generate test data
-export([gen_test_certs/1,
         x509_pkix_sign_types/1,
         root_cert/2]).

-include("public_key_internal.hrl").

%%====================================================================
%% Internal application APIs
%%====================================================================

%%====================================================================
%% Path validation
%%====================================================================

%%--------------------------------------------------------------------
-spec init_validation_state(#cert{}, integer(), list()) ->
				   #path_validation_state{}.
%%
%% Description: Creates initial version of path_validation_state for
%% basic path validation of x509 certificates.
%%--------------------------------------------------------------------
init_validation_state(Cert, DefaultPathLen,
		      Options) ->
    PolicyTree = pubkey_policy_tree:root(),
    MaxLen =  proplists:get_value(max_path_length, Options, DefaultPathLen),
    UserPolicySet = policy_set(Options, [?anyPolicy]),
    ExplicitPolicyConstraint =
        policy_indicator(MaxLen,
                         proplists:get_value(explicit_policy, Options, false)),
    AnyPolicyConstraint =
        policy_indicator(MaxLen,
                         proplists:get_value(inhibit_any_policy, Options, false)),
    PolicyMappingConstraint =
        policy_indicator(MaxLen,
                         proplists:get_value(inhibit_policy_mapping, Options, false)),
    {VerifyFun, UserState} = proplists:get_value(verify_fun, Options,
                                                 ?DEFAULT_VERIFYFUN),
    State = #path_validation_state{max_path_length         = MaxLen,
                                   user_initial_policy_set = UserPolicySet,
				   valid_policy_tree       = PolicyTree,
				   explicit_policy         = ExplicitPolicyConstraint,
				   inhibit_any_policy      = AnyPolicyConstraint,
				   inhibit_policy_mapping  = PolicyMappingConstraint,
				   verify_fun              = VerifyFun,
				   user_state	           = UserState,
				   cert_num                = 0},
    prepare_for_next_cert(Cert, State).

%%--------------------------------------------------------------------
-spec validate_extensions(#cert{}, #path_validation_state{},
			  term(), fun())->
				 {#path_validation_state{}, UserState :: term()}.
%%
%% Description: Check extensions included in basic path validation.
%%--------------------------------------------------------------------
validate_extensions(Cert, ValidationState0, UserState0, VerifyFun) ->
    OtpCert = otp_cert(Cert),
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    case TBSCert#'OTPTBSCertificate'.version of
	N when N >= 3 ->
	    Extensions = TBSCert#'OTPTBSCertificate'.extensions,
            validate_extensions(Cert, Extensions,
                                ValidationState0, no_basic_constraint,
                                is_self_signed(OtpCert), UserState0, VerifyFun);
        _ -> %% Extensions not present in versions 1 & 2
	    {ValidationState0, UserState0}
    end.
%%--------------------------------------------------------------------
-spec validate_policy_tree(#cert{}, #path_validation_state{})->
          #path_validation_state{} | no_return().
%%
%% Description: Check policy tree requirements after handling of certificate extensions
%%--------------------------------------------------------------------
validate_policy_tree(Cert,
                     #path_validation_state{explicit_policy = ExplicitPolicyConstraint,
                                            valid_policy_tree = Tree,
                                            user_state = UserState0,
                                            verify_fun = VerifyFun} =
                         ValidationState) ->
    case (ExplicitPolicyConstraint > 0) orelse not pubkey_policy_tree:is_empty(Tree) of
        true ->
            ValidationState;
        false ->
            UserState =
                verify_fun(Cert, {bad_cert,
                                  {policy_requirement_not_met,
                                   {{explicit_policy, ExplicitPolicyConstraint},
                                       {policy_set,
                                        pubkey_policy_tree:constrained_policy_node_set(Tree)}}}},
                           UserState0, VerifyFun),
            ValidationState#path_validation_state{user_state = UserState}
    end.

%%--------------------------------------------------------------------
-spec validate_time(#cert{}, term(), fun()) -> term().
%%
%% Description: Check that the certificate validity period includes the
%% current time.
%%--------------------------------------------------------------------
validate_time(Cert, UserState, VerifyFun) ->
    % Parse and check validity of the certificate dates, and if it fails, invoke `verify_fun` to
    % hand over control to the caller in order to decide what to do.
    OtpCert = otp_cert(Cert),
    case parse_and_check_validity_dates(OtpCert) of
        expired ->
            % Certificate has correctly formatted dates but it's expired
            verify_fun(Cert, {bad_cert, cert_expired}, UserState, VerifyFun);
        error ->
            % Certificate has incorrectly formatted dates, attempt to delegate decision to app function
            verify_fun(Cert, {bad_cert, invalid_validity_dates}, UserState, VerifyFun);
        % Validation succeded and certificate is not expired, no new state needed
        ok ->
            UserState
    end.

-spec parse_and_check_validity_dates(#'OTPCertificate'{}) -> ok | expired | error.
%%
%% Description: Determines if the passed certificate consains correctly
%% formatted dates in the validity field. If so, it checks if the certificate
%% is not expired. Otherwise, it returns error.
%%--------------------------------------------------------------------
parse_and_check_validity_dates(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    {'Validity', NotBeforeStr, NotAfterStr}
	= TBSCert#'OTPTBSCertificate'.validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    try
        NotBefore = time_str_2_gregorian_sec(notBefore, NotBeforeStr),
        NotAfter = time_str_2_gregorian_sec(notAfter, NotAfterStr),
        
        % Expiration check
        if
            ((NotBefore =< Now) and (Now =< NotAfter)) -> ok;
            true -> expired
        end

        % "error:function_clause" is thrown by time_str_2_gregorian_sec if the date format is not valid
        % verify_fun only throws exceptions        
    catch error:function_clause -> 
        error
    end.

%%--------------------------------------------------------------------
-spec validate_issuer(#cert{}, term(), term(), fun()) -> term() | no_return().
%%
%% Description: Check that the certificate issuer name is the working_issuer_name
%% in path_validation_state.
%%--------------------------------------------------------------------
validate_issuer(Cert, Issuer, UserState, VerifyFun) ->
    OtpCert = otp_cert(Cert),
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    case is_issuer(Issuer, TBSCert#'OTPTBSCertificate'.issuer) of
	true ->
	    UserState;
	_ ->
	    verify_fun(Cert, {bad_cert, invalid_issuer}, UserState, VerifyFun)
    end.
%%--------------------------------------------------------------------
-spec validate_names(#cert{}, no_constraints | list(), list(),
		     term(), term(), fun())-> term() | no_return().
%%
%% Description: Validate Subject Alternative Name.
%%--------------------------------------------------------------------
validate_names(Cert, Permit, Exclude, Last, UserState, VerifyFun) ->
    OtpCert = otp_cert(Cert),
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
		    verify_fun(Cert, {bad_cert, name_not_permitted},
			      UserState, VerifyFun)
	    end
    end.

%%--------------------------------------------------------------------
-spec validate_signature(#cert{}, DER::binary(),
			 term(),term(), term(), fun()) -> term() | no_return().

%%
%% Description: Check that the signature on the certificate can be verified using
%% working_public_key_algorithm, the working_public_key, and
%% the working_public_key_parameters in path_validation_state.
%%--------------------------------------------------------------------
validate_signature(Cert, DerCert, Key, KeyParams,
		   UserState, VerifyFun) ->
    OtpCert = otp_cert(Cert),
    case verify_signature(OtpCert, DerCert, Key, KeyParams) of
	true ->
	    UserState;
	false ->
	    verify_fun(Cert, {bad_cert, invalid_signature}, UserState, VerifyFun)
    end.

%%--------------------------------------------------------------------
-spec verify_data(DER::binary()) ->
           {DigestType, PlainText, Signature}
               when DigestType :: md5 | crypto:sha1() | crypto:sha2() | none,
                    PlainText  :: binary(),
                    Signature  :: binary().
%%
%% Description: Extracts data from DerCert needed to call public_key:verify/4.
%%--------------------------------------------------------------------
verify_data(DerCert) ->
    {ok, OtpCert} = pubkey_cert_records:decode_cert(DerCert),
    extract_verify_data(OtpCert, DerCert).

%%--------------------------------------------------------------------
-spec verify_fun(#cert{}, {bad_cert, public_key:bad_cert_reason()} |
                 {extension, #'Extension'{}}|
		 valid | valid_peer, term(), fun()) -> term() | no_return().
%%
%% Description: Gives the user application the opportunity handle path
%% validation errors and unknown extensions and optional do other
%% things with a validated certificate.
%% --------------------------------------------------------------------
verify_fun(#cert{der = DerCert, otp = OtpCert}, Result, UserState0, VerifyFun) ->
    case apply_fun(VerifyFun, OtpCert, DerCert, Result, UserState0) of
        {valid, UserState} ->
	    UserState;
	{valid_peer, UserState} ->
	    UserState;
	{fail, Reason} ->
	    case Reason of
		{bad_cert, _} ->
		    throw(Reason);
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
-spec prepare_for_next_cert(#cert{}, #path_validation_state{}) ->
				   #path_validation_state{} | no_return().
%%
%% Description: Update path_validation_state for next iteration.
%%--------------------------------------------------------------------
prepare_for_next_cert(Cert, #path_validation_state{policy_mapping_ext = Ext} =
                          ValidationState0) when Ext =/= undefined ->
    ValidationState1 = handle_policy_mappings(Cert, ValidationState0),
    ValidationState =
        ValidationState1#path_validation_state{policy_mapping_ext =
                                                   undefined,
                                               current_any_policy_qualifiers =
                                                   undefined},
    prepare_for_next_cert(Cert, ValidationState);
prepare_for_next_cert(Cert, #path_validation_state{
                                  working_public_key_algorithm = PrevAlgo,
                                  working_public_key_parameters =
                                      PrevParams,
                                  cert_num = CertNum,
                                  explicit_policy = ExplicitPolicyConstraint,
                                  inhibit_policy_mapping = PolicyMappingConstraint,
                                  inhibit_any_policy = AnyPolicyConstraint
                                 } = ValidationState0) ->
    OtpCert = otp_cert(Cert),
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    Issuer =  TBSCert#'OTPTBSCertificate'.subject,

    {Algorithm, PublicKey, PublicKeyParams0} =
	public_key_info(TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
			ValidationState0),
    PublicKeyParams =
	case PublicKeyParams0 of
	    'NULL' when Algorithm =:= PrevAlgo ->
		PrevParams;
	    asn1_NOVALUE when Algorithm =:= PrevAlgo ->
		PrevParams;
	    _ -> PublicKeyParams0
	end,

    IsSelfSigned = is_self_signed(OtpCert),
    ValidationState1 =
        ValidationState0#path_validation_state{
          working_public_key_algorithm = Algorithm,
          working_public_key = PublicKey,
          working_public_key_parameters = PublicKeyParams,
          working_issuer_name = Issuer,
          cert_num = CertNum + 1,
          policy_ext_present = false,
          valid_policy_tree =
              assert_valid_policy_tree(ValidationState0#path_validation_state.explicit_policy,
                                       ValidationState0#path_validation_state.policy_ext_present,
                                       ValidationState0#path_validation_state.valid_policy_tree),
          current_any_policy_qualifiers = undefined,
          policy_ext_any = undefined,

          %% 6.1.4 h from RFC
          %% Step 1 or 6.1.5 a (if last cert) from RFC
          explicit_policy = maybe_decrement(ExplicitPolicyConstraint, IsSelfSigned),
          % Step 2 from RFC
          inhibit_policy_mapping = maybe_decrement(PolicyMappingConstraint, IsSelfSigned),
          % Step 3 from RFC
          inhibit_any_policy = maybe_decrement(AnyPolicyConstraint, IsSelfSigned)
         },
    ValidationState2 = handle_policy_constraints(ValidationState1),
    ValidationState = handle_inhibit_anypolicy(ValidationState2),
    handle_last_cert(Cert, ValidationState).

apply_fun(Fun, OtpCert, DerCert, Result, UserState) ->
    if is_function(Fun, 4) ->
            Fun(OtpCert, DerCert, Result, UserState);
       is_function(Fun, 3) ->
            Fun(OtpCert, Result, UserState)
    end.


%%====================================================================
%% Utility functions
%%====================================================================

%%--------------------------------------------------------------------
-spec normalize_general_name({rdnSequence, term()}| binary()) -> {rdnSequence, term()}.
%%
%% Description: Normalizes a general name so that it can be easily
%%              compared to another general name.
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
is_issuer({rdnSequence, _} = Issuer, {rdnSequence, _} = Candidate) ->
    {rdnSequence, IssuerDirName} = normalize_general_name(Issuer),
    {rdnSequence, CandidateDirName} = normalize_general_name(Candidate),
    is_dir_name(IssuerDirName, CandidateDirName, true).
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
-spec subject_id(#'OTPCertificate'{}) ->
		       {integer(), term()}.
%%
%% Description: Extracts the subject and serial number from a certificate.
%%--------------------------------------------------------------------
subject_id(Otpcert) ->
    TBSCert = Otpcert#'OTPCertificate'.tbsCertificate,
    Subject = TBSCert#'OTPTBSCertificate'.subject,
    SerialNr = TBSCert#'OTPTBSCertificate'.serialNumber,
    {SerialNr, normalize_general_name(Subject)}.


distribution_points(Otpcert) ->
    TBSCert = Otpcert#'OTPCertificate'.tbsCertificate,
    Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    case select_extension(?'id-ce-cRLDistributionPoints', Extensions) of
	undefined ->
	    [];
	#'Extension'{extnValue = Value} ->
	    Value
    end.

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
-spec select_extension(Oid ::tuple(),[#'Extension'{}]) ->
			      #'Extension'{} | undefined.
%%
%% Description: Extracts a specific extension from a list of extensions.
%%--------------------------------------------------------------------
select_extension(_, asn1_NOVALUE) ->
    undefined;
select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = ?'id-ce-cRLDistributionPoints' = Id,
                                   extnValue = Value} = Extension | _]) when is_binary(Value) ->
    Extension#'Extension'{extnValue = public_key:der_decode('CRLDistributionPoints', Value)};
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

%%--------------------------------------------------------------------
-spec match_name(Type:: rfc822Name | directoryName | uniformResourceIdentifier |
                 emailAddress | dNSName | x400Address | ipAdress,
                 Name::term(), Names::[term()]) -> boolean().
%%
%% Description: Does <Name> match any of name in Names according to
%% the match rules for the Type.
%%--------------------------------------------------------------------
match_name(rfc822Name, Name, [PermittedName | Rest]) ->
    match_name(fun is_valid_host_or_domain/2, Name, PermittedName, Rest);

match_name(directoryName, DirName,  [PermittedName | Rest]) ->
    match_name(fun is_rdnSeq/2, DirName, PermittedName, Rest);

match_name(uniformResourceIdentifier, URI,  [PermittedName | Rest]) ->
    case uri_string:normalize(URI, [return_map]) of
	#{host := Host} ->
	    PN = case uri_string:normalize(PermittedName, [return_map]) of
		     #{host := PNhost} -> PNhost;
		     _X -> PermittedName
		 end,
	    match_name(fun is_valid_host_or_domain/2, Host, PN, Rest);
        _ ->
            false
    end;

match_name(emailAddress, Name, [PermittedName | Rest]) ->
    Fun = fun(Email, PermittedEmail) ->
                  is_valid_email_address(Email, PermittedEmail,
                                         string:tokens(PermittedEmail,"@"))
          end,
    match_name(Fun, Name, PermittedName, Rest);

match_name(dNSName, Name, [PermittedName | Rest]) ->
    Fun = fun(Domain, [$.|Domain]) -> true;
	     (Name1, [$. | _] = Name2) ->
                  is_suffix(Name2, Name1);
             (Name1, Name2) ->
                  StrLen1 = string:len(Name1),
                  StrLen2 = string:len(Name2),
                  case StrLen1 > StrLen2 of
                      true ->
                          is_suffix([$. | Name2], Name1);
                      false when StrLen1 == StrLen2 ->
                          string:casefold(Name1) == string:casefold(Name2);
                      false ->
                          false
                  end
          end,
    match_name(Fun, Name, PermittedName, Rest);

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

%%====================================================================
%% Generate test data
%%====================================================================

%%--------------------------------------------------------------------
-spec gen_test_certs(#{server_chain:= public_key:chain_opts(),
                       client_chain:= public_key:chain_opts()} |
                     public_key:chain_opts()) ->
                            public_key:test_config() |
                            [public_key:conf_opt()].
%% Description: Generates server and and client configuration for testing
%% purposes. All certificate options have default values
%%--------------------------------------------------------------------
gen_test_certs(
  #{client_chain :=
        #{root := ClientRoot,
          intermediates := ClientCAs,
          peer := ClientPeer},
    server_chain :=
        #{root := ServerRoot,
          intermediates := ServerCAs,
          peer := ServerPeer}}) ->
    #{cert := ServerRootCert, key := ServerRootKey} =
        case ServerRoot of
            #{} ->
                ServerRoot;
            ServerRootConf when is_list(ServerRootConf) ->
                root_cert("SERVER ROOT CA", ServerRootConf)
        end,
    #{cert := ClientRootCert, key := ClientRootKey} =
        case ClientRoot of
            #{} ->
                ClientRoot;
            ClientRootConf when is_list(ClientRootConf) ->
                root_cert("CLIENT ROOT CA", ClientRootConf)
        end,
    [{ServerDERCert, ServerDERKey} | ServerCAsKeys] =
        config(
          server, ServerRootCert, ServerRootKey,
          lists:reverse([ServerPeer | lists:reverse(ServerCAs)])),
    [{ClientDERCert, ClientDERKey} | ClientCAsKeys] =
        config(
          client, ClientRootCert, ClientRootKey,
          lists:reverse([ClientPeer | lists:reverse(ClientCAs)])),
    ServerDERCA = ca_config(ClientRootCert, ServerCAsKeys),
    ClientDERCA = ca_config(ServerRootCert, ClientCAsKeys),
    #{server_config =>
          [{cert, ServerDERCert}, {key, ServerDERKey},
           {cacerts, ServerDERCA}],
      client_config =>
          [{cert, ClientDERCert}, {key, ClientDERKey},
           {cacerts, ClientDERCA}]};
%%
%% Generates a node configuration for testing purposes,
%% when using the node server cert also for the client.
%% All certificate options have default values
gen_test_certs(
  #{root := Root, intermediates := CAs, peer := Peer}) ->
    #{cert := RootCert, key := RootKey} =
        case Root of
            #{} ->
                Root;
            RootConf when is_list(RootConf) ->
                root_cert("SERVER ROOT CA", RootConf)
        end,
    [{DERCert, DERKey} | CAsKeys] =
        config(
          server, RootCert, RootKey,
          lists:reverse([Peer | lists:reverse(CAs)])),
    DERCAs = ca_config(RootCert, CAsKeys),
    [{cert, DERCert}, {key, DERKey}, {cacerts, DERCAs}].

%%%%--------------------------------------------------------------------
-spec x509_pkix_sign_types(#'SignatureAlgorithm'{}) -> {Hash::atom(), Sign::atom(),
                                                        Options::list()}.
%%
%% Description: Extract signature algorithm options.
%%%%--------------------------------------------------------------------
x509_pkix_sign_types(
  #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                        parameters = #'RSASSA-PSS-params'{
                                        saltLength = SaltLen,
                                        hashAlgorithm = #'HashAlgorithm'{algorithm = Alg}}}) ->
    Hash = public_key:pkix_hash_type(Alg),
    {Hash, rsa_pss_pss, [{rsa_padding, rsa_pkcs1_pss_padding},
                         {rsa_pss_saltlen, SaltLen},
                         {rsa_mgf1_md, Hash}]};
x509_pkix_sign_types(#'SignatureAlgorithm'{algorithm = Alg}) ->
    {Hash, Sign} = public_key:pkix_sign_types(Alg),
    {Hash, Sign, []}.

%%%%--------------------------------------------------------------------
-spec root_cert(string(), [public_key:cert_opt()]) -> public_key:test_root_cert().
%%
%% Description: Generate a self-signed root cert
%%%%--------------------------------------------------------------------
root_cert(Name, Opts) ->
    PrivKey = gen_key(proplists:get_value(key, Opts, default_key_gen())),
    TBS = cert_template(),
    Issuer = subject("root", Name),
    SignatureId =  sign_algorithm(PrivKey, Opts),
    SPI = public_key(PrivKey, SignatureId),

    OTPTBS =
        TBS#'OTPTBSCertificate'{
          signature = SignatureId,
          issuer = Issuer,
          validity = validity(Opts),
          subject = Issuer,
          subjectPublicKeyInfo = SPI,
          extensions = extensions(undefined, ca, Opts)
         },
    #{cert => public_key:pkix_sign(OTPTBS, PrivKey),
      key => PrivKey}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% No extensions present
validate_extensions(Cert, asn1_NOVALUE, ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    validate_extensions(Cert, [], ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(Cert, [], ValidationState, basic_constraint, _SelfSigned,
		    UserState0, VerifyFun) ->
    UserState = validate_ext_key_usage(Cert, UserState0, VerifyFun, ca),
    {ValidationState, UserState};
validate_extensions(Cert, [], ValidationState =
			#path_validation_state{max_path_length = Len,
					       last_cert = Last},
		    no_basic_constraint, SelfSigned, UserState0, VerifyFun) ->
    case Last of
	true when SelfSigned ->
	    {ValidationState, UserState0};
	true  ->
            UserState = validate_ext_key_usage(Cert, UserState0, VerifyFun, endentity),
	    {ValidationState#path_validation_state{max_path_length = Len - 1},
	     UserState};
	false ->
	    %% basic_constraint must appear in certs used for digital sign
	    %% see 4.2.1.10 in rfc 3280
	    case is_digitally_sign_cert(Cert) of
		true ->
		    missing_basic_constraints(Cert, SelfSigned,
					      ValidationState, VerifyFun,
					      UserState0, Len);
		false -> %% Example CRL signer only
		    {ValidationState, UserState0}
	    end
    end;
validate_extensions(Cert,
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
    validate_extensions(Cert, Rest,
			ValidationState#path_validation_state{max_path_length =
								  Length},
			basic_constraint, SelfSigned,
			UserState, VerifyFun);
%% The pathLenConstraint field is meaningful only if cA is set to
%% TRUE.
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-basicConstraints',
					   extnValue =
					       #'BasicConstraints'{cA = false}} |
			      Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);

validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-keyUsage',
                                        extnValue = KeyUses
                                       } | Rest],
		    #path_validation_state{last_cert=Last} = ValidationState,
		    ExistBasicCon, SelfSigned,
		    UserState0, VerifyFun) ->
    case Last orelse lists:member(keyCertSign, KeyUses) of
	true ->
	    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState0, VerifyFun);
	false ->
	    UserState = verify_fun(Cert, {bad_cert, invalid_key_usage},
				   UserState0, VerifyFun),
	    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState, VerifyFun)
    end;

validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-subjectAltName',
					   extnValue = Names,
					   critical = true} = Ext | Rest],
		    ValidationState, ExistBasicCon,
		    SelfSigned, UserState0, VerifyFun)  ->
    case validate_subject_alt_names(Names) of
	true  ->
	    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState0, VerifyFun);
	false ->
	    UserState = verify_fun(Cert, {extension, Ext},
				   UserState0, VerifyFun),
	    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon,
				SelfSigned, UserState, VerifyFun)
    end;

validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-nameConstraints',
				  extnValue = NameConst} | Rest],
		    ValidationState,
		    ExistBasicCon, SelfSigned, UserState, VerifyFun) ->
    Permitted = NameConst#'NameConstraints'.permittedSubtrees,
    Excluded = NameConst#'NameConstraints'.excludedSubtrees,

    NewValidationState = add_name_constraints(Permitted, Excluded,
					      ValidationState),

    validate_extensions(Cert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-certificatePolicies',
					   extnValue = Info}
			      | Rest],
                    ValidationState,
		    ExistBasicCon, SelfSigned, UserState, VerifyFun) ->
    Tree = process_policy_tree(Info, SelfSigned, ValidationState),
    validate_extensions(Cert, Rest,
			ValidationState#path_validation_state{
                          policy_ext_present = true,
                          current_any_policy_qualifiers =
                              current_any_policy_qualifiers(Info),
			  valid_policy_tree = Tree},
			ExistBasicCon, SelfSigned, UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-policyConstraints'} = Ext
			      | Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    NewValidationState = ValidationState#path_validation_state{policy_constraint_ext = Ext},
    validate_extensions(Cert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-policyMappings'} = Ext
                             | Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    NewValidationState = ValidationState#path_validation_state{policy_mapping_ext = Ext},
    validate_extensions(Cert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-inhibitAnyPolicy'} = Ext
			      | Rest], ValidationState, ExistBasicCon,
		    SelfSigned, UserState, VerifyFun) ->
    NewValidationState = ValidationState#path_validation_state{policy_inhibitany_ext = Ext},
    validate_extensions(Cert, Rest, NewValidationState, ExistBasicCon,
			SelfSigned, UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{extnID = ?'id-ce-extKeyUsage',
                                           critical = true,
                                           extnValue = ExtKeyUse} = Extension | Rest],
		    #path_validation_state{last_cert = false} = ValidationState, ExistBasicCon,
		    SelfSigned, UserState0, VerifyFun) ->
    UserState =
        case ext_keyusage_includes_any(ExtKeyUse) of
            true -> %% CA cert that specifies ?anyExtendedKeyUsage should not be marked critical
                verify_fun(Cert, {bad_cert, invalid_ext_key_usage}, UserState0, VerifyFun);
            false ->
                case ca_known_extend_key_use(ExtKeyUse) of
                    true ->
                        UserState0;
                    false ->
                        verify_fun(Cert, {extension, Extension}, UserState0, VerifyFun)
                end
        end,
    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon, SelfSigned,
			UserState, VerifyFun);
validate_extensions(Cert, [#'Extension'{} = Extension | Rest],
		    ValidationState, ExistBasicCon,
		    SelfSigned, UserState0, VerifyFun) ->
    UserState = verify_fun(Cert, {extension, Extension}, UserState0, VerifyFun),
    validate_extensions(Cert, Rest, ValidationState, ExistBasicCon, SelfSigned,
			UserState, VerifyFun).

handle_last_cert(Cert, #path_validation_state{last_cert = true,
                                                 user_initial_policy_set = PolicySet,
                                                 valid_policy_tree = Tree} = ValidationState0) ->
    OtpCert = otp_cert(Cert),
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions =
        extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    %% 6.1.5 b
    ValidationState  =
        case select_extension(?'id-ce-policyConstraints', Extensions) of
            undefined ->
                ValidationState0;
            #'Extension'{extnValue = #'PolicyConstraints'{requireExplicitPolicy = 0}} ->
                ValidationState0#path_validation_state{explicit_policy = 0};
            _  ->
                ValidationState0
    end,
    ValidTree = policy_tree_intersection(PolicySet, Tree),
    validate_policy_tree(Cert,
                         ValidationState#path_validation_state{valid_policy_tree = ValidTree});
handle_last_cert(_, ValidationState) ->
    ValidationState.

validate_ext_key_usage(#cert{otp = OtpCert} = Cert, UserState, VerifyFun, Type) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    KeyUseExt = pubkey_cert:select_extension(?'id-ce-keyUsage', Extensions),
    ExtKeyUseExt =  pubkey_cert:select_extension(?'id-ce-extKeyUsage', Extensions),
    case compatible_ext_key_usage(KeyUseExt, ExtKeyUseExt, Type) of
        true ->
            UserState;
        false ->
            verify_fun(Cert, {bad_cert, {key_usage_mismatch, {KeyUseExt, ExtKeyUseExt}}},
                       UserState, VerifyFun)
    end.

%%====================================================================
%% Policy handling
%%====================================================================
%% Start initialization  RFC 5280 Section 6.1.2 ----------------------

%%  6.1.2 d, e, f: If <indicator> is set, then the initial value
%%  is 0, otherwise the initial value is n+1. (N = max path length)
policy_indicator(_, true) ->
    0;
policy_indicator(N, false) ->
   N + 1.

policy_set(Opts, Default) ->
    case proplists:get_value(policy_set, Opts, undefined) of
        undefined ->
            Default;
        Set ->
            [oidify(OidStr) || OidStr <- Set]
    end.

oidify(Oid) when is_tuple(Oid) ->
    Oid;
oidify(Oid) when  is_list(Oid) ->
    Tokens = string:tokens(Oid, "$."),
    OidList = [list_to_integer(StrInt) || StrInt <- Tokens],
    list_to_tuple(OidList).

%% End initialization ----------------------------------------------------------

%% Start Basic Policy Processing RFC 5280 Section 6.1.3 -----------------------

%% 6.1.3 f
assert_valid_policy_tree(0, PresentPolicyExtension, Tree) ->
    assert_valid_policy_tree(PresentPolicyExtension, Tree);
assert_valid_policy_tree(_, _, Tree) ->
    Tree.

assert_valid_policy_tree(undefined, Tree) ->
    Tree; %% Initial tree, happens when called in init_validation_state/3
assert_valid_policy_tree(true, Tree) ->
    Tree; %% Policy extension present in step n
assert_valid_policy_tree(false, _Tree) -> % 6.1.3 e
    %% Policy extension missing in step n, tree becomes empty
    pubkey_policy_tree:empty().

%% 6.1.3 d: If the certificate policies extension is present in the
%% certificate and the valid_policy_tree is not NULL, process the
%% policy information by performing the following steps in order:
process_policy_tree(PolicyInformation, SelfSigned,
                    #path_validation_state{valid_policy_tree = Tree0} =
                        ValidationState) ->  
    case pubkey_policy_tree:is_empty(Tree0) of
        true ->
            Tree0;
        false ->
            %% Step 1 & 2
            Tree = add_policy_children(PolicyInformation,
                                       SelfSigned, ValidationState),
            %% Step 3: If there is a node in the valid_policy_tree of depth i-1 or
            %% less without any child nodes, delete that node.  Repeat this step
            %% until there are no nodes of depth i-1 or less without children.
            pubkey_policy_tree:prune_tree(Tree) 
    end.

%% 6.1.3 d
add_policy_children(PolicyInfoList0, SelfSigned,
                    #path_validation_state{valid_policy_tree = Tree0,
                                           inhibit_any_policy = AnyPolicyConstraint,
                                           cert_num = CertNum,
                                           max_path_length = PathLen
                                          }) ->
    {AnyExt, PolicyInfoList} =
        case lists:keytake(?anyPolicy,
                           #'PolicyInformation'.policyIdentifier, PolicyInfoList0) of
            {value, AnyExt0, PolicyInfoList1} ->
                {AnyExt0, PolicyInfoList1};
            false ->
                {undefined, PolicyInfoList0}
        end,
    %% Step 1
    %% i
    LeafFun =
        fun(#{expected_policy_set := ExpPolicySet}) ->
                policy_children(ExpPolicySet, PolicyInfoList)
        end,
    Tree1 = pubkey_policy_tree:add_leaves(Tree0, LeafFun),
    
    %% posibly ii
    AllLeaves = pubkey_policy_tree:all_leaves(Tree1),
    Siblings = fun(#{valid_policy := ?anyPolicy}) ->
                       any_policy_children(AllLeaves, PolicyInfoList);
                  (_) -> []
               end,
    Tree = pubkey_policy_tree:add_leaf_siblings(Tree1, Siblings),
    %% Step 2
    handle_any_ext(Tree, AnyExt, AnyPolicyConstraint, SelfSigned, CertNum, PathLen).

%% 6.1.3 - d 1 i
%% Step 1: For each policy P not equal to anyPolicy in the certificate
%%  policies extension, let P-OID denote the OID for policy P and P-Q
%%  denote the qualifier set for policy P.  Perform the following
%%  steps in order:

%%   (i) For each node of depth i-1 in the valid_policy_tree where
%%   P-OID is in the expected_policy_set, create a child node as
%%   follows: set the valid_policy to P-OID, set the qualifier_set to
%%   P-Q, and set the expected_policy_set to {P-OID}.
policy_children(ExpPolicySet, PolicyInfoList) ->
    lists:foldl(fun(#'PolicyInformation'{
                       policyIdentifier = Policy,
                        policyQualifiers = Qualifiers
                      }, Acc0
                   ) ->
                        case lists:member(Policy, ExpPolicySet) of
                            true ->
                                [pubkey_policy_tree:policy_node(Policy, Qualifiers, [Policy]) | Acc0];
                            false  ->
                                Acc0
                        end
                end, [], PolicyInfoList).

%% 6.1.3 - d 1 ii
%% If there was no match in step (i) and the valid_policy_tree
%% includes a node of depth i-1 with the valid_policy anyPolicy,
%% generate a child node with the following values: set the
%% valid_policy to P-OID, set the qualifier_set to P-Q, and set the
%% expected_policy_set to {P-OID}.
any_policy_children([], PolicyInfoList) ->
    lists:foldl(fun(#'PolicyInformation'{
                       policyIdentifier = Policy,
                       policyQualifiers = Qualifiers
                      }, Acc0
                   ) ->
                        Node = pubkey_policy_tree:policy_node(Policy, Qualifiers, [Policy]),
                        [Node | Acc0]
                end, [], PolicyInfoList);
any_policy_children(_, _) ->
    no_sibling.

%% 6.1.3 - 2 d 
%%   For each node in the valid_policy_tree of depth i-1, for each
%%   value in the expected_policy_set (including anyPolicy) that does
%%   not appear in a child node, create a child node with the
%%   following values: set the valid_policy to the value from the
%%   expected_policy_set in the parent node, set the qualifier_set to
%%   AP-Q, and set the expected_policy_set to the value in the
%%   valid_policy from this node.
handle_any_ext(Tree, undefined, _, _, _,_) ->
    Tree;
handle_any_ext(Tree, #'PolicyInformation'{
                           policyIdentifier = ?anyPolicy,
                           policyQualifiers = Qualifiers}, AnyPolicyConstraint,
               SelfSigned, CertNum, PathLen) ->
    case AnyPolicyConstraint > 0 orelse
        ((CertNum < PathLen) andalso SelfSigned) of
        true ->
            AllLeaves = pubkey_policy_tree:all_leaves(Tree),
            Siblings = fun(Node) ->
                               any_ext_policy_children(Node, Qualifiers, AllLeaves)
                       end,
            pubkey_policy_tree:add_leaf_siblings(Tree, Siblings);
        false ->
            Tree
    end.

any_ext_policy_children(#{expected_policy_set := ExpPolicySet}, Qualifiers, AllLeaves) ->
    [pubkey_policy_tree:policy_node(Policy, Qualifiers, [Policy])
     || Policy <- ExpPolicySet, not pubkey_policy_tree:in_set(Policy, AllLeaves)
    ].

%% End Basic Policy Processing -------------------------------------------------

%% Start Prepare Next Cert Policy Handling  RFC 5280 Section 6.1.4 -------------

%% 6.1.4. b start:
handle_policy_mappings(Cert,
                       #path_validation_state{valid_policy_tree = Tree0,
                                              policy_mapping_ext =
                                                  #'Extension'{extnID = ?'id-ce-policyMappings',
                                                               extnValue = PolicyMappings}}
                       = ValidationState) ->
    case handle_policy_mappings(PolicyMappings, Cert, Tree0, ValidationState) of
        {tree, Tree} ->
            ValidationState#path_validation_state{valid_policy_tree = Tree};
        {user_state, UState} ->
            ValidationState#path_validation_state{user_state = UState}
    end.

handle_policy_mappings([], _, Tree, _) ->
    {tree, Tree};
handle_policy_mappings([Mappings | Rest], Cert, Tree0, ValidationState) ->
    case handle_policy_mapping(Mappings, Cert, Tree0, ValidationState) of
        {tree, Tree} ->
            handle_policy_mappings(Rest, Cert, Tree, ValidationState);
        Other ->
            Other
    end.

%% 6.1.4. a: If a policy mappings extension is present, verify that the
%% special value anyPolicy does not appear as an issuerDomainPolicy or
%% a subjectDomainPolicy.
handle_policy_mapping(#'PolicyMappings_SEQOF'{
                         issuerDomainPolicy =
                             IssuerPolicy,
                         subjectDomainPolicy =
                             SubjectPolicy} = Ext,
                      Cert, Tree0,
                      #path_validation_state{inhibit_policy_mapping =
                                                 PolicyMappingConstraint,
                                             current_any_policy_qualifiers =
                                                 AnyQualifiers,
                                             verify_fun = VerifyFun,
                                             user_state = UserState}
                     ) ->
    case not (?anyPolicy == IssuerPolicy) andalso
        not (?anyPolicy == SubjectPolicy) of
        true ->
            Tree = handle_policy_mapping_ext(Ext, Tree0,
                                             PolicyMappingConstraint, AnyQualifiers),
            {tree, Tree};
        false ->
            UserState = verify_fun(Cert, {bad_cert, {invalid_policy_mapping, Ext}},
                                   UserState, VerifyFun),
            {user_state, UserState}
    end.

%% 6.1.4. b continue:
handle_policy_mapping_ext(#'PolicyMappings_SEQOF'{
                         issuerDomainPolicy =
                             IssuerPolicy},
                         Tree0, 0, _) -> %% 6.1.4. b 2:
    %% (2) If the policy_mapping variable is equal to 0:

    %% (i) delete each node of depth i in the valid_policy_tree where
    %% ID-P is the valid_policy.
    %%
    %% (ii) If there is a node in the valid_policy_tree of depth i-1
    %% or less without any child nodes, delete that node.  Repeat this
    %% step until there are no nodes of depth i-1 or less without
    %% children.

    Tree = pubkey_policy_tree:prune_leaves(Tree0, IssuerPolicy),
    pubkey_policy_tree:prune_tree(Tree);
handle_policy_mapping_ext(#'PolicyMappings_SEQOF'{
                             issuerDomainPolicy = IssuerPolicy,
                             subjectDomainPolicy = SubjectPolicy},
                          Tree, N, AnyQualifiers) when N > 0 -> %% 6.1.4. b 1:
   
    %% (1) If the policy_mapping variable is greater than 0, for each
    %% node in the valid_policy_tree of depth i where ID-P is the
    %% valid_policy, set expected_policy_set to the set of
    %% subjectDomainPolicy values that are specified as equivalent to
    %% ID-P by the policy mappings extension.
    MapPolicy =
        fun(#{valid_policy := ValidPolicy, expected_policy_set := Set} = Node)
              when ValidPolicy == IssuerPolicy ->
                case Set of %% Initial policy should be mapped over, but can be mapped to itself
                    [ValidPolicy] when ValidPolicy =/= SubjectPolicy ->
                        Node#{expected_policy_set => [SubjectPolicy]};
                    _ ->
                        %% Avoid duplicating self mapping
                        case lists:member(SubjectPolicy, Set) of
                            true ->
                                Node;
                            false ->
                                Node#{expected_policy_set => Set ++ [SubjectPolicy]}
                        end
                end;
           (Node) ->
                Node
        end,

    %% If no node of depth i in the valid_policy_tree has a
    %% valid_policy of ID-P but there is a node of depth i with a
    %% valid_policy of anyPolicy, then generate a child node of the
    %% node of depth i-1 that has a valid_policy of anyPolicy as
    %% follows:

    %% (i) set the valid_policy to ID-P;

    %% (ii) set the qualifier_set to the qualifier set of the policy
    %% anyPolicy in the certificate policies extension of certificate
    %% i; and

    %% (iii) set the expected_policy_set to the set of
    %% subjectDomainPolicy values that are specified as equivalent to
    %% ID-P by the policy mappings extension.    
    AnySiblings = fun(#{valid_policy := ?anyPolicy}) ->
                          [pubkey_policy_tree:policy_node(IssuerPolicy,
                                                          AnyQualifiers,
                                                          [SubjectPolicy])];
                     (_) ->
                         no_sibling
                  end,

    case pubkey_policy_tree:map_leaves(Tree, MapPolicy) of
        Tree -> %% If no policy was mapped!
            pubkey_policy_tree:add_leaf_siblings(Tree, AnySiblings);
        NewTree ->
            NewTree
    end.

%% 6.1.4 i
handle_policy_constraints(#path_validation_state{
                             policy_constraint_ext =
                                 #'Extension'{extnID = ?'id-ce-policyConstraints',
                                              extnValue =
                                                  #'PolicyConstraints'{requireExplicitPolicy =
                                                                           ExplicitPolicy,
                                                                       inhibitPolicyMapping =
                                                                           InhibitMapPolicy}},
                             explicit_policy = CurrentExplicitPolicyConstraint,
                             inhibit_policy_mapping = CurrentPolicyMappingConstraint} =
                              ValidationState) ->
    ExplicitPolicyConstraint =
        policy_constraint(CurrentExplicitPolicyConstraint, ExplicitPolicy), % Step 1
    PolicyMappingConstraint =
        policy_constraint(CurrentPolicyMappingConstraint, InhibitMapPolicy), % Step 2
    ValidationState#path_validation_state{explicit_policy = ExplicitPolicyConstraint,
                                          inhibit_policy_mapping = PolicyMappingConstraint,
                                          policy_constraint_ext = undefined};
handle_policy_constraints(ValidationState) ->
    ValidationState.

%% 6.4.1 j
handle_inhibit_anypolicy(#path_validation_state{policy_inhibitany_ext =
                                                    #'Extension'{extnID = ?'id-ce-inhibitAnyPolicy',
                                                                 extnValue = InhibitAnyPolicy
                                                                },
                                                inhibit_any_policy = CurrentAnyPolicy} =
                             ValidationState) ->
    AnyPolicyConstraint = policy_constraint(CurrentAnyPolicy, InhibitAnyPolicy),
    ValidationState#path_validation_state{inhibit_any_policy = AnyPolicyConstraint,
                                          policy_inhibitany_ext = undefined};
handle_inhibit_anypolicy(ValidationState) ->
    ValidationState.

policy_constraint(Current, asn1_NOVALUE) ->
    Current;
policy_constraint(Current, New) ->
    erlang:min(Current, New).

current_any_policy_qualifiers(Info) ->
    case lists:keyfind(?anyPolicy, #'PolicyInformation'.policyIdentifier, Info) of
        #'PolicyInformation'{policyQualifiers = AnyQualifiers} ->
            AnyQualifiers;
        _ ->
            []
    end.

maybe_decrement(0, _) ->
    0;
maybe_decrement(N, false) ->
    N-1;
maybe_decrement(N, true) ->
    N.

%% End Prepare Next Cert Policy Handling ---------------------------------------

%% Start Wrap Up Policy Handling RFC 5280 Section 6.1.5 %% ---------------------

%% Step G from RFC

policy_tree_intersection([?anyPolicy], Tree) -> % (ii) from RFC
    Tree;
policy_tree_intersection(UserPolicySet, Tree0) ->
    case pubkey_policy_tree:is_empty(Tree0) of
        true ->  % (i) from RFC
            Tree0;
        false -> % (iii) from RFC
            %% Step 1 from RFC
            ValidPolicyNodeSet = pubkey_policy_tree:valid_policy_node_set(Tree0),
 
            %% Step 2 from RFC
            InvalidNodes = apply_user_constraints(ValidPolicyNodeSet, UserPolicySet),
            Tree1 = pubkey_policy_tree:prune_invalid_nodes(Tree0, InvalidNodes),

            %% Step 3 from RFC
            Tree = handle_any_policy_leaves(Tree1, ValidPolicyNodeSet, UserPolicySet),

            %% Step 4 from RFC
            pubkey_policy_tree:prune_tree(Tree)
    end.

apply_user_constraints(_, [?anyPolicy]) ->
    [];
apply_user_constraints(ValidPolicyNodeSet, UserPolicySet) ->
    apply_user_constraints(ValidPolicyNodeSet, UserPolicySet, []).

apply_user_constraints([], _, Acc) ->
    Acc;
apply_user_constraints([#{valid_policy := ?anyPolicy} | Rest],
               UserPolicySet, Acc) ->
    apply_user_constraints(Rest, UserPolicySet, Acc);
apply_user_constraints([#{valid_policy := Policy} = Node | Rest],
                UserPolicySet, Acc) ->
    case lists:member(Policy, UserPolicySet) of
        true ->
            apply_user_constraints(Rest, UserPolicySet, Acc);
        false ->
            apply_user_constraints(Rest, UserPolicySet, [Node | Acc])
    end.

handle_any_policy_leaves(Tree, _, [?anyPolicy]) ->
    Tree;
handle_any_policy_leaves(Tree0, ValidPolicyNodeSet, UserPolicySet) ->
    case pubkey_policy_tree:any_leaves(Tree0) of
        [] ->
            Tree0;
        AnyLeaves ->
            Tree = add_policy_nodes(AnyLeaves, Tree0, ValidPolicyNodeSet, UserPolicySet),
            pubkey_policy_tree:prune_leaves(Tree, ?anyPolicy)
    end.

add_policy_nodes([], Tree, _, _) ->
    Tree;
add_policy_nodes([#{qualifier_set := Qualifiers} | Rest], Tree0,
                 ValidPolicyNodeSet, UserPolicySet) ->
    PolicySet = [UPolicy ||  UPolicy <- UserPolicySet,
                             not pubkey_policy_tree:in_set(UPolicy, ValidPolicyNodeSet)],
    Children =
        [pubkey_policy_tree:policy_node(Policy, Qualifiers, [Policy]) || Policy <- PolicySet],
    Siblings = fun(#{valid_policy := ?anyPolicy, qualifier_set := QSet}) when QSet == Qualifiers->
                       Children;
                  (_) -> []
               end,
    add_policy_nodes(Rest, pubkey_policy_tree:add_leaf_siblings(Tree0, Siblings),
                     ValidPolicyNodeSet, UserPolicySet).

%% End Wrap Up Policy Handling -------------------------------------------------

%%====================================================================
%% Date handling
%%====================================================================


%% time_str_2_gregorian_sec/2 is a wrapper (decorator pattern) over
%% time_str_2_gregorian_sec/1. the decorator deals with notBefore and notAfter
%% property differently when we pass utcTime because the data format is
%% ambiguous YYMMDD. on generalTime the year ambiguity cannot happen because
%% years are expressed in a 4-digit format, i.e., YYYYMMDD.
-spec time_str_2_gregorian_sec(PeriodOfTime, Time) -> Seconds :: non_neg_integer() when
      PeriodOfTime :: notBefore | notAfter,
      Time :: {utcTime | generalTime, [non_neg_integer() | char()]}.
time_str_2_gregorian_sec(notBefore, {utcTime, [FirstDigitYear | _]=UtcTime}) ->
    %% To be compliant with PKITS Certification Path Validation,
    %% we must accept certificates with notBefore = 50, meaning 1950.
    %% Once the PKITS certification path validation is updated,
    %% we must update this function body and test case
    %% {"4.2.3", "Valid pre2000 UTC notBefore Date Test3 EE"}
    %% in pkits_SUITE.erl
    Y1 = erlang:list_to_integer([FirstDigitYear]),
    YearPrefix = case (Y1 > 4 andalso Y1 =< 9) of
                     true -> [$1, $9];
                     false  ->
                         {Y, _M, _D} = erlang:date(),
                         integer_to_list(Y div 100)
                 end,
    time_str_2_gregorian_sec({generalTime, YearPrefix ++ UtcTime});

time_str_2_gregorian_sec(notAfter, {utcTime, UtcTime}) ->
    SlidingDate = sliding_year_window(UtcTime),
    time_str_2_gregorian_sec({generalTime, SlidingDate});

time_str_2_gregorian_sec(_, {generalTime, _Time}=GeneralTime) ->
    time_str_2_gregorian_sec(GeneralTime).

%% converts 'Time' as a string into gregorian time in seconds.
-spec time_str_2_gregorian_sec(Time) -> Seconds :: non_neg_integer() when
      Time :: {generalTime | utcTime, string()}.
time_str_2_gregorian_sec({utcTime, UtcTime}) ->
    time_str_2_gregorian_sec(notAfter, {utcTime, UtcTime});

time_str_2_gregorian_sec({generalTime,[Y1,Y2,Y3,Y4,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,$Z]}) ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
					    {Hour, Min, Sec}}).

%% Sliding window algorithm to calculate the time.
%% The value is set as taking {Y1, Y2} from the first two digits of
%% current_date - 50 or current_date - 49.
sliding_year_window([Y1,Y2,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,Z]) ->
    {{CurrentYear,_, _}, _} = calendar:universal_time(),
    LastTwoDigitYear = CurrentYear rem 100,
    MinYear = mod(LastTwoDigitYear - 50, 100),
    YearWindow = case list_to_integer([Y1,Y2]) of
                     N when N < MinYear -> CurrentYear + 50;
                     N when N >= MinYear -> CurrentYear - 49
                 end,
    [Year1, Year2] = integer_to_list(YearWindow div 100),
    [Year1,Year2,Y1,Y2,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,Z].


%% Helper function to perform modulo calculation for integer
-spec mod(A :: integer(), B :: non_neg_integer()) -> non_neg_integer().
mod(A, B) when A > 0 -> A rem B;
mod(A, B) when A < 0 -> mod(A+B, B);
mod(0, _) -> 0.

%%====================================================================
%% Name handling
%%====================================================================
match_name(Fun, Name, PermittedName, []) ->
    Fun(Name, PermittedName);
match_name(Fun, Name, PermittedName, [Head | Tail]) ->
    case Fun(Name, PermittedName) of
	true ->
	    true;
	false ->
	    match_name(Fun, Name, Head, Tail)
    end.

do_normalize_general_name(Issuer) ->
    Normalize = fun([{Description, Type, {printableString, Value}}]) ->
			NewValue = string:casefold(strip_spaces(Value, false)),
			[{Description, Type, {printableString, NewValue}}];
		   (Atter)  ->
			Atter
		end,
    lists:map(Normalize, Issuer).

%% See rfc3280 4.1.2.6 Subject: regarding emails.
extract_email({rdnSequence, List}) ->
    extract_email2(List).
extract_email2([[#'AttributeTypeAndValue'{type=?'id-emailAddress',
					  value=Mail}]|_]) ->
    [{rfc822Name, Mail}];
extract_email2([_|Rest]) ->
    extract_email2(Rest);
extract_email2([]) -> [].

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
is_dir_name(_A,_B,_) ->
    false.

%% attribute values in types other than PrintableString are case
%% sensitive (this permits matching of attribute values as binary
%% objects); that is term comparison will compare. Rules origninate
%% from RFC 3280 section 4.1.24. However fallback to case insensite
%% matching also for utf8 strings, as this is done by the
%% pkits_suite interop suite
is_dir_name2(Str, Str) ->
    true;
is_dir_name2({T1, Str1}, Str2)
  when T1 == printableString; T1 == utf8String ->
    is_dir_name2(Str1, Str2);
is_dir_name2(Str1, {T2, Str2})
  when T2 == printableString; T2 == utf8String ->
    is_dir_name2(Str1, Str2);
is_dir_name2(Str1, Str2)
  when (is_list(Str1) orelse is_binary(Str1)) andalso
       (is_list(Str2) orelse is_binary(Str2)) ->
    %%attribute values in PrintableString are compared after
    %%removing leading and trailing white space and converting internal
    %%substrings of one or more consecutive white space characters to a
    %%single space. They are case insensetive.
    string:equal(strip_spaces(Str1, true), strip_spaces(Str2, true), true);
is_dir_name2(_, _) ->
    false.

strip_spaces(String0, KeepDeep) ->
    Trimmed = string:trim(String0),
    strip_many_spaces(string:split(Trimmed, "  ", all), KeepDeep).

strip_many_spaces([OnlySingleSpace], _) ->
    OnlySingleSpace;
strip_many_spaces(Strings, KeepDeep) ->
    Split = [string:trim(Str, leading, " ") || Str <- Strings, Str /= []],
    DeepList = lists:join(" ", Split),
    case KeepDeep of
        true -> DeepList;
        false -> unicode:characters_to_list(DeepList)
    end.

decode_general_name([{directoryName, Issuer}]) ->
    normalize_general_name(Issuer);
decode_general_name([{_, Issuer}]) ->
    Issuer.

cert_auth_key_id(#'AuthorityKeyIdentifier'{authorityCertIssuer
					   = asn1_NOVALUE}) ->
    {error, issuer_not_found};
cert_auth_key_id(#'AuthorityKeyIdentifier'{authorityCertIssuer =
					   AuthCertIssuer,
					   authorityCertSerialNumber =
					   SerialNr}) ->
    {ok, {SerialNr, decode_general_name(AuthCertIssuer)}}.

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

is_valid_uri(AbsURI) ->
    case uri_string:normalize(AbsURI, [return_map]) of
        #{scheme := _} ->
            true;
        _ ->
            false
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

is_valid_host_or_domain([], _) ->
    false; %% Can happen if URI was not a HTTP URI
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
    lists:suffix(string:casefold(Suffix), string:casefold(Str)).

case_insensitive_match(Str1, Str2) ->
    string:equal(Str1, Str2, true).

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

%%====================================================================
%% Signature handling
%%====================================================================

extract_verify_data(OtpCert, DerCert) ->
    Signature = OtpCert#'OTPCertificate'.signature,
    SigAlg = OtpCert#'OTPCertificate'.signatureAlgorithm,
    PlainText = encoded_tbs_cert(DerCert),
    {DigestType,_,_} = x509_pkix_sign_types(SigAlg),
    {DigestType, PlainText, Signature}.

verify_signature(OtpCert, DerCert, Key, KeyParams) ->
    {DigestType, PlainText, Signature} = extract_verify_data(OtpCert, DerCert),
    case Key of
	#'RSAPublicKey'{} ->
            case KeyParams of
                #'RSASSA-PSS-params'{} ->
                    public_key:verify(PlainText, DigestType, Signature, Key,
                                      verify_options(KeyParams));
                'NULL' ->
                    public_key:verify(PlainText, DigestType, Signature, Key);
                asn1_NOVALUE ->
                    public_key:verify(PlainText, DigestType, Signature, Key)
            end;
	_ ->
	    public_key:verify(PlainText, DigestType, Signature, {Key, KeyParams})
    end.

encoded_tbs_cert(Cert) ->
    {ok, PKIXCert} = 'OTP-PKIX':decode_TBSCert_exclusive(Cert),
    {'OTPCertificate',
     {'OTPCertificate_tbsCertificate', EncodedTBSCert}, _, _} = PKIXCert,
    EncodedTBSCert.

public_key_info(PublicKeyInfo,
		#path_validation_state{working_public_key_algorithm =
				       WorkingAlgorithm,
				       working_public_key_parameters =
				       WorkingParams}) ->
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey=PublicKey,
                               algorithm=AlgInfo} = PublicKeyInfo,

    PublicKeyParams = AlgInfo#'PublicKeyAlgorithm'.parameters,
    Algorithm = AlgInfo#'PublicKeyAlgorithm'.algorithm,

    NewPublicKeyParams =
	case PublicKeyParams of
	    {null, 'NULL'} when WorkingAlgorithm == Algorithm ->
		WorkingParams;
            asn1_NOVALUE when Algorithm == ?'id-Ed25519';
                              Algorithm == ?'id-Ed448' ->
                {namedCurve, Algorithm};
            {params, Params} ->
		Params;
            Params ->
		Params
	end,
    {Algorithm, PublicKey, NewPublicKeyParams}.


extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.

is_fixed_dh_cert(PublicKeyInfo, Extensions) ->
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    Algorithm = AlgInfo#'PublicKeyAlgorithm'.algorithm,
   
    case select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	    is_dh(Algorithm);
	#'Extension'{extnValue = KeyUses} ->
	    is_dh(Algorithm) andalso lists:member(keyAgreement, KeyUses)
    end.
	
is_dh(?'dhpublicnumber')->
    true;
is_dh(_) ->
    false.

is_digitally_sign_cert(Cert) ->
    OtpCert = otp_cert(Cert),
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    case pubkey_cert:select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	     false;
	#'Extension'{extnValue = KeyUse} ->
	    lists:member(keyCertSign, KeyUse)
    end.

compatible_ext_key_usage(undefined, _, endentity) ->
    true;
compatible_ext_key_usage(_, undefined, _) ->
    true;
compatible_ext_key_usage(#'Extension'{extnID = ?'id-ce-keyUsage',
                                      extnValue = KeyUses},
                         #'Extension'{extnID = ?'id-ce-extKeyUsage',
                                      extnValue = Purposes}, Type) ->
    case ext_keyusage_includes_any(Purposes) of
        true ->
            true;
        false ->
            is_compatible_purposes(KeyUses, Purposes, Type)
    end.

is_compatible_purposes(_, [], _) ->
    true;
is_compatible_purposes(KeyUses, [?'id-kp-serverAuth'| Rest], ca = Type) ->
    %% keyCertSign is already verified for a ca and considered compatible
    is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [?'id-kp-serverAuth'| Rest], endentity = Type) ->
    IsServerAuthComp = case lists:member(digitalSignature, KeyUses) of
                           true ->
                               true;
                           false ->
                               lists:member(keyAgreement, KeyUses)  orelse
                                   lists:member(keyEncipherment, KeyUses)
                       end,
    IsServerAuthComp andalso is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [?'id-kp-clientAuth'| Rest], ca = Type) ->
    %% keyCertSign is already verified for a ca and considered compatible
    is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [?'id-kp-clientAuth'| Rest], endentity = Type) ->
    IsClientAuthComp = case lists:member(digitalSignature, KeyUses) of
                           true ->
                               true;
                           false ->
                               lists:member(keyAgreement, KeyUses)
                       end,
    IsClientAuthComp andalso is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [?'id-kp-codeSigning'| Rest], Type) ->
    lists:member(digitalSignature, KeyUses) andalso
        is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [?'id-kp-emailProtection'| Rest], Type) ->
    IsEmailProtCompatible = case (lists:member(digitalSignature, KeyUses) orelse
                                  lists:member(nonRepudiation, KeyUses)) of
                                true ->
                                    true;
                                false ->
                                    lists:member(keyAgreement, KeyUses) orelse
                                        lists:member(keyEncipherment, KeyUses)
                            end,
    IsEmailProtCompatible andalso is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [Id| Rest],Type) when Id == ?'id-kp-timeStamping';
                                                      Id == ?'id-kp-OCSPSigning'->
    (lists:member(digitalSignature, KeyUses) orelse
     lists:member(nonRepudiation, KeyUses)) andalso
        is_compatible_purposes(KeyUses, Rest, Type);
is_compatible_purposes(KeyUses, [_| Rest], Type) -> %% Unknown purposes are for user verify_fun to care about
    is_compatible_purposes(KeyUses, Rest, Type).


ca_known_extend_key_use(ExtKeyUses) ->
    CAExtSet = ca_known_ext_key_usage(),
    Intersection = sets:intersection(CAExtSet, sets:from_list(ExtKeyUses)),
    not sets:is_empty(Intersection).

ca_known_ext_key_usage() ->
    %% Following extended key usages are known
    sets:from_list([?'id-kp-serverAuth', ?'id-kp-clientAuth',
                    ?'id-kp-codeSigning', ?'id-kp-emailProtection',
                    ?'id-kp-timeStamping', ?'id-kp-OCSPSigning']).

missing_basic_constraints(OtpCert, SelfSigned, ValidationState, VerifyFun, UserState0,Len) ->
    UserState = verify_fun(OtpCert, {bad_cert, missing_basic_constraint},
			   UserState0, VerifyFun),
    case SelfSigned of
	true ->
	    {ValidationState, UserState};
	false ->
	    {ValidationState#path_validation_state{max_path_length =
						       Len - 1},
	     UserState}
    end.

%%====================================================================
%% Generate test data
%%====================================================================

gen_key(KeyGen) ->
     case is_key(KeyGen) of
         true ->
             KeyGen;
         false ->
             public_key:generate_key(KeyGen)
     end.

is_key(#'DSAPrivateKey'{}) ->
    true;
is_key(#'RSAPrivateKey'{}) ->
    true;
is_key({#'RSAPrivateKey'{}, _}) ->
    true;
is_key(#'ECPrivateKey'{}) ->
    true;
is_key(_) ->
    false.


cert_template() ->
    #'OTPTBSCertificate'{
       version = v3,
       serialNumber = erlang:unique_integer([positive, monotonic]),
       issuerUniqueID = asn1_NOVALUE,
       subjectUniqueID = asn1_NOVALUE
      }.

subject(Contact, Name) ->
    Opts = [{email, Contact ++ "@example.org"},
	    {name,  Name},
	    {city, "Stockholm"},
	    {country, "SE"},
	    {org, "erlang"},
	    {org_unit, "automated testing"}],
    subject(Opts).

subject(SubjectOpts) when is_list(SubjectOpts) ->
    Encode = fun(Opt) ->
		     {Type,Value} = subject_enc(Opt),
		     [#'AttributeTypeAndValue'{type=Type, value=Value}]
	     end,
    {rdnSequence, [Encode(Opt) || Opt <- SubjectOpts]}.

subject_enc({name,  Name}) ->
    {?'id-at-commonName', {printableString, Name}};
subject_enc({email, Email}) ->
    {?'id-emailAddress', Email};
subject_enc({city,  City}) ->
    {?'id-at-localityName', {printableString, City}};
subject_enc({org, Org}) ->
    {?'id-at-organizationName', {printableString, Org}};
subject_enc({org_unit, OrgUnit}) ->
    {?'id-at-organizationalUnitName', {printableString, OrgUnit}};
subject_enc({country, Country}) ->
    {?'id-at-countryName', Country}.

validity(Opts) ->
    DefFrom0 = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())-1),
    DefTo0   = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())+7),
    {DefFrom, DefTo} = proplists:get_value(validity, Opts, {DefFrom0, DefTo0}),

    GenFormat =
        fun({Y,M,D}) ->
                lists:flatten(
                  io_lib:format("~4..0w~2..0w~2..0w130000Z",[Y,M,D]))
        end,

    UTCFormat =
        fun({Y,M,D}) ->
                [_, _, Y3, Y4] = integer_to_list(Y),
                lists:flatten(
                  io_lib:format("~s~2..0w~2..0w130000Z",[[Y3, Y4],M,D]))
        end,
    #'Validity'{notBefore = validity_format(DefFrom, GenFormat, UTCFormat),
                notAfter = validity_format(DefTo, GenFormat, UTCFormat)}.

validity_format({Year, _, _} = Validity, GenFormat, _UTCFormat) when Year >= 2049 ->
    {generalTime, GenFormat(Validity)};
validity_format(Validity, _GenFormat, UTCFormat) ->
    {utcTime, UTCFormat(Validity)}.


sign_algorithm(#'RSAPrivateKey'{} = Key , Opts) ->
      case proplists:get_value(rsa_padding, Opts, rsa_pkcs1_pss_padding) of
        rsa_pkcs1_pss_padding ->
            DigestId = rsa_digest_oid(proplists:get_value(digest, Opts, sha1)),
            rsa_sign_algo(Key, DigestId, asn1_NOVALUE);
        rsa_pss_rsae ->
            DigestId = rsa_digest_oid(proplists:get_value(digest, Opts, sha256)),
            rsa_sign_algo(Key, DigestId, asn1_NOVALUE)
      end;
sign_algorithm({#'RSAPrivateKey'{} = Key,#'RSASSA-PSS-params'{} = Params}, _Opts) ->
    rsa_sign_algo(Key, ?'id-RSASSA-PSS', Params);

sign_algorithm(#'DSAPrivateKey'{p=P, q=Q, g=G}, _Opts) ->
    #'SignatureAlgorithm'{algorithm  = ?'id-dsa-with-sha1',
                          parameters = {params,#'Dss-Parms'{p=P, q=Q, g=G}}};
sign_algorithm(#'ECPrivateKey'{parameters = {namedCurve, EDCurve}}, _Opts)
  when EDCurve == ?'id-Ed25519';
       EDCurve == ?'id-Ed448' ->
    #'SignatureAlgorithm'{algorithm  = EDCurve,
                          parameters = asn1_NOVALUE};
sign_algorithm(#'ECPrivateKey'{parameters = Parms}, Opts) ->
    Type = ecdsa_digest_oid(proplists:get_value(digest, Opts, sha1)),
    #'SignatureAlgorithm'{algorithm  = Type,
                          parameters = Parms}.

rsa_sign_algo(#'RSAPrivateKey'{}, ?'id-RSASSA-PSS' = Type,  #'RSASSA-PSS-params'{} = Params) ->
    #'SignatureAlgorithm'{algorithm  = Type,
                          parameters = Params};
rsa_sign_algo(#'RSAPrivateKey'{}, Type, Parms) ->
    #'SignatureAlgorithm'{algorithm  = Type,
                          parameters = Parms}.

rsa_digest_oid(Oid) when is_tuple(Oid) ->
    Oid;
rsa_digest_oid(sha1) ->
    ?'sha1WithRSAEncryption';
rsa_digest_oid(sha) ->
    ?'sha1WithRSAEncryption';
rsa_digest_oid(sha512) ->
    ?'sha512WithRSAEncryption';
rsa_digest_oid(sha384) ->
    ?'sha384WithRSAEncryption';
rsa_digest_oid(sha256) ->
    ?'sha256WithRSAEncryption';
rsa_digest_oid(md5) ->
    ?'md5WithRSAEncryption'.

ecdsa_digest_oid(Oid) when is_tuple(Oid) ->
    Oid;
ecdsa_digest_oid(sha1) ->
    ?'ecdsa-with-SHA1';
ecdsa_digest_oid(sha) ->
    ?'ecdsa-with-SHA1';
ecdsa_digest_oid(sha512) ->
    ?'ecdsa-with-SHA512';
ecdsa_digest_oid(sha384) ->
    ?'ecdsa-with-SHA384';
ecdsa_digest_oid(sha256) ->
    ?'ecdsa-with-SHA256'.

config(Role, Root, Key, Opts) ->
   cert_chain(Role, Root, Key, Opts).

cert_chain(Role, Root, RootKey, Opts) ->
    cert_chain(Role, Root, RootKey, Opts, 0, []).

cert_chain(Role, IssuerCert, IssuerKey, [PeerOpts], _, Acc) ->
    Key = gen_key(proplists:get_value(key, PeerOpts, default_key_gen())),
    Cert = cert(Role, public_key:pkix_decode_cert(IssuerCert, otp),
                IssuerKey, Key, "admin", " Peer cert", PeerOpts, peer),
    [{Cert, encode_key(Key)}, {IssuerCert, encode_key(IssuerKey)} | Acc];
cert_chain(Role, IssuerCert, IssuerKey, [CAOpts | Rest], N, Acc) ->
    Key = gen_key(proplists:get_value(key, CAOpts, default_key_gen())),
    Cert = cert(Role, public_key:pkix_decode_cert(IssuerCert, otp), IssuerKey, Key, "webadmin",
                " Intermediate CA " ++ integer_to_list(N), CAOpts, ca),
    cert_chain(Role, Cert, Key, Rest, N+1, [{IssuerCert, encode_key(IssuerKey)} | Acc]).

cert(Role, #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Issuer}},
     PrivKey, Key, Contact, Name, Opts, Type) ->
    TBS = cert_template(),
    SignAlgoId = sign_algorithm(PrivKey, Opts),
    OTPTBS = TBS#'OTPTBSCertificate'{
               signature = SignAlgoId,
               issuer =  Issuer,
               validity = validity(Opts),
               subject = subject(Contact, atom_to_list(Role) ++ Name),
               subjectPublicKeyInfo = public_key(Key, SignAlgoId),
               extensions = extensions(Role, Type, Opts)
              },
    public_key:pkix_sign(OTPTBS, PrivKey).

ca_config(Root, CAsKeys) ->
    [Root | [CA || {CA, _}  <- CAsKeys]].

default_key_gen() ->
    case crypto:ec_curves() of
        [] ->
            {rsa, 2048, 17};
        [Curve |_] ->
            Oid = pubkey_cert_records:namedCurves(Curve),
            {namedCurve, Oid}
    end.

public_key(#'RSAPrivateKey'{modulus=N, publicExponent=E},
           #'SignatureAlgorithm'{algorithm  = ?rsaEncryption,
                                 parameters = #'RSASSA-PSS-params'{} = Params}) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters = Params},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = Public};
public_key({#'RSAPrivateKey'{modulus=N, publicExponent=E}, #'RSASSA-PSS-params'{} = Params},
           #'SignatureAlgorithm'{algorithm  = ?'id-RSASSA-PSS',
                                 parameters = #'RSASSA-PSS-params'{} = Params}) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-RSASSA-PSS', parameters= Params},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = Public};
public_key(#'RSAPrivateKey'{modulus=N, publicExponent=E}, _) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters=asn1_NOVALUE},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = Public};
public_key(#'DSAPrivateKey'{p=P, q=Q, g=G, y=Y}, _) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-dsa',
				 parameters={params, #'Dss-Parms'{p=P, q=Q, g=G}}},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo, subjectPublicKey = Y};
public_key(#'ECPrivateKey'{version = _Version,
                           privateKey = _PrivKey,
                           parameters = {namedCurve, ?'id-Ed25519' = ID},
                           publicKey = PubKey}, _) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ID, parameters=asn1_NOVALUE},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = #'ECPoint'{point = PubKey}};
public_key(#'ECPrivateKey'{version = _Version,
                           privateKey = _PrivKey,
                           parameters = {namedCurve, ?'id-Ed448' = ID},
                           publicKey = PubKey}, _) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ID, parameters=asn1_NOVALUE},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = #'ECPoint'{point = PubKey}};
public_key(#'ECPrivateKey'{version = _Version,
                           privateKey = _PrivKey,
                           parameters = Params,
                           publicKey = PubKey}, _) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-ecPublicKey', parameters=Params},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = #'ECPoint'{point = PubKey}}.

extensions(Role, Type, Opts) ->
    Exts  = proplists:get_value(extensions, Opts, []),
    add_default_extensions(Role, Type, Exts).

add_default_extensions(_, ca, Exts) ->
    Default = [#'Extension'{extnID = ?'id-ce-keyUsage',
                            extnValue = [keyCertSign, digitalSignature, cRLSign],
                            critical = false},
               #'Extension'{extnID = ?'id-ce-basicConstraints',
                            extnValue = #'BasicConstraints'{cA = true},
                            critical = true}],
    add_default_extensions(Default, Exts);

add_default_extensions(server, peer, Exts) ->
    Hostname = net_adm:localhost(),
    Default = [#'Extension'{extnID = ?'id-ce-keyUsage',
                            extnValue = [digitalSignature, keyAgreement],
                            critical = false},
               #'Extension'{extnID = ?'id-ce-subjectAltName',
                            extnValue = [{dNSName, Hostname}],
                            critical = false}
              ],
    add_default_extensions(Default, Exts);
add_default_extensions(client, peer, Exts) ->
    Default = [#'Extension'{extnID = ?'id-ce-keyUsage',
                            extnValue = [digitalSignature],
                            critical = false}
              ],
    add_default_extensions(Default, Exts).

add_default_extensions(Defaults0, Exts) ->
    Defaults = lists:filtermap(fun(#'Extension'{extnID = ID} = Ext) ->
                                       case lists:keymember(ID, 2, Exts) of
                                           true ->
                                               false;
                                           false ->
                                               {true, Ext}
                                       end
                               end, Defaults0),
    Exts ++ Defaults.

encode_key({#'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}} = Key) ->
    {Asn1Type, DER, _} = public_key:pem_entry_encode('PrivateKeyInfo', Key),
    {Asn1Type, DER};
encode_key(#'RSAPrivateKey'{} = Key) ->
    {'RSAPrivateKey', public_key:der_encode('RSAPrivateKey', Key)};
encode_key(#'ECPrivateKey'{} = Key) ->
    {'ECPrivateKey', public_key:der_encode('ECPrivateKey', Key)};
encode_key(#'DSAPrivateKey'{} = Key) ->
    {'DSAPrivateKey', public_key:der_encode('DSAPrivateKey', Key)}.

verify_options(
  #'RSASSA-PSS-params'{saltLength = SaltLen,
                       maskGenAlgorithm =
                           #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                               parameters =
                                                   #'HashAlgorithm'{algorithm = HashOid}}}) ->
    HashAlgo = public_key:pkix_hash_type(HashOid),
    [{rsa_padding, rsa_pkcs1_pss_padding},
     {rsa_pss_saltlen, SaltLen},
     {rsa_mgf1_md, HashAlgo}].

ext_keyusage_includes_any(KeyUses) when is_list(KeyUses) ->
    lists:member(?anyExtendedKeyUsage, KeyUses);
ext_keyusage_includes_any(_) ->
    false.

otp_cert(Der) when is_binary(Der) ->
    public_key:pkix_decode_cert(Der, otp);
otp_cert(#'OTPCertificate'{} = Cert) ->
    Cert;
otp_cert(#cert{otp = OtpCert}) ->
    OtpCert.
