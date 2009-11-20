%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

-module(pubkey_cert_records).

-include("public_key.hrl").

-export([decode_cert/2, encode_cert/1, encode_tbs_cert/1]).

-export([old_decode_cert/2, old_encode_cert/1]).  %% Debugging and testing new code.

%%====================================================================
%% Internal application API
%%====================================================================

decode_cert(DerCert, plain) ->
    'OTP-PUB-KEY':decode('Certificate', DerCert);
decode_cert(DerCert, otp) ->
    {ok, Cert} = 'OTP-PUB-KEY':decode('OTPCertificate', DerCert),
    {ok, decode_all_otp(Cert)}.

old_decode_cert(DerCert, otp) ->
    {ok, Cert} = 'OTP-PUB-KEY':decode('Certificate', DerCert),
    {ok, plain_to_otp(Cert)}.

old_encode_cert(Cert) ->
    PlainCert = otp_to_plain(Cert),
    {ok, EncCert} = 'OTP-PUB-KEY':encode('Certificate', PlainCert),
    list_to_binary(EncCert).


encode_cert(Cert = #'Certificate'{}) ->
    {ok, EncCert} = 'OTP-PUB-KEY':encode('Certificate', Cert),
    list_to_binary(EncCert);
encode_cert(C = #'OTPCertificate'{tbsCertificate = TBS = 
				  #'OTPTBSCertificate'{
						     issuer=Issuer0,
						     subject=Subject0,
						     subjectPublicKeyInfo=Spki0,
						     extensions=Exts0}
				 }) ->
    Issuer  = transform(Issuer0,encode),
    Subject = transform(Subject0,encode),
    Spki = encode_supportedPublicKey(Spki0),
    Exts = encode_extensions(Exts0),
    %%    io:format("Extensions ~p~n",[Exts]),
    Cert = C#'OTPCertificate'{tbsCertificate=
			      TBS#'OTPTBSCertificate'{
				issuer=Issuer, subject=Subject,
				subjectPublicKeyInfo=Spki,
				extensions=Exts}},
    {ok, EncCert} = 'OTP-PUB-KEY':encode('OTPCertificate', Cert),
    list_to_binary(EncCert).

encode_tbs_cert(TBS = #'OTPTBSCertificate'{
		  issuer=Issuer0,
		  subject=Subject0,
		  subjectPublicKeyInfo=Spki0,
		  extensions=Exts0}) ->
    Issuer  = transform(Issuer0,encode),
    Subject = transform(Subject0,encode),
    Spki = encode_supportedPublicKey(Spki0),
    Exts = encode_extensions(Exts0),
    TBSCert = TBS#'OTPTBSCertificate'{issuer=Issuer,subject=Subject,
				      subjectPublicKeyInfo=Spki,extensions=Exts},
    {ok, EncTBSCert} = 'OTP-PUB-KEY':encode('OTPTBSCertificate', TBSCert),
    list_to_binary(EncTBSCert).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

decode_all_otp(C = #'OTPCertificate'{tbsCertificate = TBS = 
				     #'OTPTBSCertificate'{
							issuer=Issuer0,
							subject=Subject0,
							subjectPublicKeyInfo=Spki0,
							extensions=Exts0}
				    }) -> 
    Issuer  = transform(Issuer0,decode),
    Subject = transform(Subject0,decode),
    Spki = decode_supportedPublicKey(Spki0),
    Exts = decode_extensions(Exts0),
    %%    io:format("Extensions ~p~n",[Exts]),
    C#'OTPCertificate'{tbsCertificate=
		       TBS#'OTPTBSCertificate'{
			 issuer=Issuer, subject=Subject,
			 subjectPublicKeyInfo=Spki,extensions=Exts}}.


%%% SubjectPublicKey
supportedPublicKeyAlgorithms(?'rsaEncryption') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-dsa') -> 'DSAPublicKey';
supportedPublicKeyAlgorithms(?'dhpublicnumber') -> 'DHPublicKey';
supportedPublicKeyAlgorithms(?'id-keyExchangeAlgorithm') -> 'KEA-PublicKey';
supportedPublicKeyAlgorithms(?'id-ecPublicKey') -> 'ECPoint'.

decode_supportedPublicKey(#'OTPSubjectPublicKeyInfo'{algorithm= PA =
						  #'PublicKeyAlgorithm'{algorithm=Algo},
						  subjectPublicKey = {0,SPK0}}) ->
    Type = supportedPublicKeyAlgorithms(Algo),
    {ok, SPK} = 'OTP-PUB-KEY':decode(Type, SPK0),
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = SPK, algorithm=PA}.

encode_supportedPublicKey(#'OTPSubjectPublicKeyInfo'{algorithm= PA =
						     #'PublicKeyAlgorithm'{algorithm=Algo},
						     subjectPublicKey = SPK0}) ->
    Type = supportedPublicKeyAlgorithms(Algo),
    {ok, SPK} = 'OTP-PUB-KEY':encode(Type, SPK0),
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = {0,list_to_binary(SPK)}, algorithm=PA}.

%%% Extensions

extension_id(?'id-ce-authorityKeyIdentifier') ->  'AuthorityKeyIdentifier';
extension_id(?'id-ce-subjectKeyIdentifier') ->    'SubjectKeyIdentifier';
extension_id(?'id-ce-keyUsage') -> 	          'KeyUsage';
extension_id(?'id-ce-privateKeyUsagePeriod') ->   'PrivateKeyUsagePeriod';
extension_id(?'id-ce-certificatePolicies') -> 	  'CertificatePolicies';
extension_id(?'id-ce-policyMappings') -> 	  'PolicyMappings';
extension_id(?'id-ce-subjectAltName') -> 	  'SubjectAltName'; 	        
extension_id(?'id-ce-issuerAltName') -> 	  'IssuerAltName'; 	        
extension_id(?'id-ce-subjectDirectoryAttributes') -> 	  'SubjectDirectoryAttributes';
extension_id(?'id-ce-basicConstraints' ) -> 	  'BasicConstraints';	        
extension_id(?'id-ce-nameConstraints') -> 	  'NameConstraints'; 	        
extension_id(?'id-ce-policyConstraints') -> 	  'PolicyConstraints'; 	
extension_id(?'id-ce-cRLDistributionPoints') ->   'CRLDistributionPoints'; 	
extension_id(?'id-ce-extKeyUsage') -> 	          'ExtKeyUsageSyntax'; 	        
extension_id(?'id-ce-inhibitAnyPolicy') -> 	  'InhibitAnyPolicy'; 	        
extension_id(?'id-ce-freshestCRL') -> 	          'FreshestCRL';
%% Missing in public_key doc 
extension_id(?'id-pe-authorityInfoAccess') -> 	  'AuthorityInfoAccessSyntax';
extension_id(?'id-pe-subjectInfoAccess') -> 	  'SubjectInfoAccessSyntax';
extension_id(?'id-ce-cRLNumber') -> 	          'CRLNumber';
extension_id(?'id-ce-issuingDistributionPoint') -> 'IssuingDistributionPoint';
extension_id(?'id-ce-deltaCRLIndicator') -> 	   'BaseCRLNumber';
extension_id(?'id-ce-cRLReasons') -> 	          'CRLReason';
extension_id(?'id-ce-certificateIssuer') -> 	  'CertificateIssuer';
extension_id(?'id-ce-holdInstructionCode') -> 	  'HoldInstructionCode';
extension_id(?'id-ce-invalidityDate') -> 	  'InvalidityDate';
extension_id(_) ->
    undefined.
     

decode_extensions(asn1_NOVALUE) ->
    asn1_NOVALUE;

decode_extensions(Exts) ->
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
		      case extension_id(Id) of
			  undefined -> Ext;
			  Type ->
			      {ok, Value} = 'OTP-PUB-KEY':decode(Type, list_to_binary(Value0)),
			      Ext#'Extension'{extnValue=transform(Value,decode)}
		      end
	      end, Exts).

encode_extensions(asn1_NOVALUE) ->
    asn1_NOVALUE;

encode_extensions(Exts) ->
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
		      case extension_id(Id) of
			  undefined -> Ext;			  
			  Type ->
			      Value1 = transform(Value0,encode),
			      {ok, Value} = 'OTP-PUB-KEY':encode(Type, Value1),
			      Ext#'Extension'{extnValue=list_to_binary(Value)}
		      end
	      end, Exts).

transform(#'AttributeTypeAndValue'{type=Id,value=Value0} = ATAV, Func) ->
    {ok, Value} =
        case attribute_type(Id) of
            Type when is_atom(Type) -> 'OTP-PUB-KEY':Func(Type, Value0);
            _UnknownType            -> {ok, Value0}
        end,
    ATAV#'AttributeTypeAndValue'{value=Value};
transform(AKI = #'AuthorityKeyIdentifier'{authorityCertIssuer=ACI},Func) ->
    AKI#'AuthorityKeyIdentifier'{authorityCertIssuer=transform(ACI,Func)};
transform(List = [{directoryName, _}],Func) ->
    [{directoryName, transform(Value,Func)} || {directoryName, Value} <- List];
transform({directoryName, Value},Func) ->
    {directoryName, transform(Value,Func)};
transform({rdnSequence, SeqList},Func) when is_list(SeqList) ->
    {rdnSequence, 
     lists:map(fun(Seq) -> 
		       lists:map(fun(Element) -> transform(Element,Func) end, Seq)
	       end, SeqList)};
%% transform(List = [{rdnSequence, _}|_],Func)  ->
%%     lists:map(fun(Element) -> transform(Element,Func) end, List);
transform(#'NameConstraints'{permittedSubtrees=Permitted, excludedSubtrees=Excluded}, Func) ->
    Res = #'NameConstraints'{permittedSubtrees=transform_sub_tree(Permitted,Func),
			     excludedSubtrees=transform_sub_tree(Excluded,Func)},
%%    io:format("~p~n",[Res]),
    Res;
transform(Other,_) ->
    Other.
transform_sub_tree(asn1_NOVALUE,_) -> asn1_NOVALUE;
transform_sub_tree(TreeList,Func) ->
    [Tree#'GeneralSubtree'{base=transform(Name,Func)} || 
	Tree = #'GeneralSubtree'{base=Name} <- TreeList].

attribute_type(?'id-at-name') -> 'X520name';
attribute_type(?'id-at-surname') -> 'X520name';
attribute_type(?'id-at-givenName') -> 'X520name';
attribute_type(?'id-at-initials') -> 'X520name';
attribute_type(?'id-at-generationQualifier') -> 'X520name';
attribute_type(?'id-at-commonName') -> 'X520CommonName';
attribute_type(?'id-at-localityName') -> 'X520LocalityName';
attribute_type(?'id-at-stateOrProvinceName') -> 'X520StateOrProvinceName';
attribute_type(?'id-at-organizationName') -> 'X520OrganizationName';
attribute_type(?'id-at-organizationalUnitName') -> 'X520OrganizationalUnitName';
attribute_type(?'id-at-title') -> 'X520Title';
attribute_type(?'id-at-dnQualifier') -> 'X520dnQualifier';
attribute_type(?'id-at-countryName') -> 'X520countryName';
attribute_type(?'id-at-serialNumber') -> 'X520SerialNumber';
attribute_type(?'id-at-pseudonym') -> 'X520Pseudonym';
attribute_type(?'id-domainComponent') -> 'DomainComponent';
attribute_type(?'id-emailAddress') -> 'EmailAddress';
attribute_type(Type) -> Type.

%%% Old code transforms

plain_to_otp(#'Certificate'{tbsCertificate = TBSCert,
			    signatureAlgorithm = SigAlg,
			    signature = Signature} = Cert) ->
    Cert#'Certificate'{tbsCertificate = plain_to_otp(TBSCert), 
		       signatureAlgorithm = plain_to_otp(SigAlg),
		       signature = plain_to_otp(Signature)};

plain_to_otp(#'TBSCertificate'{signature = Signature,
			       issuer = Issuer,
			       subject = Subject,
			       subjectPublicKeyInfo = SPubKeyInfo,
			       extensions = Extensions} = TBSCert) ->
    
    TBSCert#'TBSCertificate'{signature = plain_to_otp(Signature),
			     issuer = plain_to_otp(Issuer),
			     subject = 
			     plain_to_otp(Subject),
			     subjectPublicKeyInfo = 
			     plain_to_otp(SPubKeyInfo),
			     extensions = 
			     plain_to_otp_extensions(Extensions)
			    };

plain_to_otp(#'AlgorithmIdentifier'{algorithm = Algorithm, 
				    parameters = Params}) ->
    SignAlgAny = 
	#'SignatureAlgorithm-Any'{algorithm = Algorithm, 
				  parameters = Params},
    {ok, AnyEnc} = 'OTP-PUB-KEY':encode('SignatureAlgorithm-Any', 
					SignAlgAny),
    {ok, SignAlg} = 'OTP-PUB-KEY':decode('SignatureAlgorithm', 
					list_to_binary(AnyEnc)),
    SignAlg;

plain_to_otp({rdnSequence, SeqList}) when is_list(SeqList) ->
    {rdnSequence, 
     lists:map(fun(Seq) -> 
		       lists:map(fun(Element) -> 
					 plain_to_otp(Element)
				 end,
				 Seq)
	       end, SeqList)};

plain_to_otp(#'AttributeTypeAndValue'{} = ATAV) ->
    {ok, ATAVEnc} = 
	'OTP-PUB-KEY':encode('AttributeTypeAndValue', ATAV),
    {ok, ATAVDec} = 'OTP-PUB-KEY':decode('OTPAttributeTypeAndValue', 
				      list_to_binary(ATAVEnc)),
    #'AttributeTypeAndValue'{type = ATAVDec#'OTPAttributeTypeAndValue'.type,
			     value =  
			     ATAVDec#'OTPAttributeTypeAndValue'.value};

plain_to_otp(#'SubjectPublicKeyInfo'{algorithm = 
				     #'AlgorithmIdentifier'{algorithm 
							    = Algo,
							    parameters = 
							    Params},
				     subjectPublicKey = PublicKey}) ->
    
    AnyAlgo = #'PublicKeyAlgorithm'{algorithm = Algo, 
				    parameters = Params},
    {0, AnyKey} = PublicKey,
    AnyDec = #'OTPSubjectPublicKeyInfo-Any'{algorithm = AnyAlgo,
					      subjectPublicKey = AnyKey},
    {ok, AnyEnc} = 
	'OTP-PUB-KEY':encode('OTPSubjectPublicKeyInfo-Any', AnyDec),
    {ok, InfoDec} = 'OTP-PUB-KEY':decode('OTPOLDSubjectPublicKeyInfo', 
					 list_to_binary(AnyEnc)),

    AlgorithmDec = InfoDec#'OTPOLDSubjectPublicKeyInfo'.algorithm,
    AlgoDec = AlgorithmDec#'OTPOLDSubjectPublicKeyInfo_algorithm'.algo, 
    NewParams = AlgorithmDec#'OTPOLDSubjectPublicKeyInfo_algorithm'.parameters,
    PublicKeyDec = InfoDec#'OTPOLDSubjectPublicKeyInfo'.subjectPublicKey,
    NewAlgorithmDec = 
	#'SubjectPublicKeyInfoAlgorithm'{algorithm = AlgoDec, 
					  parameters = NewParams},
    #'SubjectPublicKeyInfo'{algorithm = NewAlgorithmDec,
			    subjectPublicKey = PublicKeyDec
			   };

plain_to_otp(#'Extension'{extnID = ExtID, 
			  critical = Critical,
			  extnValue = Value}) 
  when  ExtID == ?'id-ce-authorityKeyIdentifier';
	ExtID == ?'id-ce-subjectKeyIdentifier';
	ExtID == ?'id-ce-keyUsage';
	ExtID == ?'id-ce-privateKeyUsagePeriod';
	ExtID == ?'id-ce-certificatePolicies';
	ExtID == ?'id-ce-policyMappings';
	ExtID == ?'id-ce-subjectAltName';
	ExtID == ?'id-ce-issuerAltName';
	ExtID == ?'id-ce-subjectDirectoryAttributes';
	ExtID == ?'id-ce-basicConstraints';
	ExtID == ?'id-ce-nameConstraints';
	ExtID == ?'id-ce-policyConstraints';
	ExtID == ?'id-ce-extKeyUsage';
	ExtID == ?'id-ce-cRLDistributionPoints';
	ExtID == ?'id-ce-inhibitAnyPolicy';
	ExtID == ?'id-ce-freshestCRL' ->    
    ExtAny = #'Extension-Any'{extnID = ExtID,
			      critical = Critical,
			      extnValue = Value},
    {ok, AnyEnc} = 'OTP-PUB-KEY':encode('Extension-Any', ExtAny),
    {ok, ExtDec} =  'OTP-PUB-KEY':decode('OTPExtension', 
				      list_to_binary(AnyEnc)),
    
    ExtValue = plain_to_otp_extension_value(ExtID, 
					    ExtDec#'OTPExtension'.extnValue),
    #'Extension'{extnID = ExtID,
		 critical = ExtDec#'OTPExtension'.critical,
		 extnValue = ExtValue};

plain_to_otp(#'Extension'{} = Ext) ->
    Ext;

plain_to_otp(#'AuthorityKeyIdentifier'{} = Ext) ->
    CertIssuer = Ext#'AuthorityKeyIdentifier'.authorityCertIssuer,
    Ext#'AuthorityKeyIdentifier'{authorityCertIssuer = 
				 plain_to_otp(CertIssuer)};


plain_to_otp([{directoryName, Value}]) ->
    [{directoryName, plain_to_otp(Value)}];

plain_to_otp(Value) ->
    Value.

otp_to_plain(#'Certificate'{tbsCertificate = TBSCert,
			    signatureAlgorithm = SigAlg,
			    signature = Signature} = Cert) ->
    Cert#'Certificate'{tbsCertificate = otp_to_plain(TBSCert), 
		       signatureAlgorithm = 
		       otp_to_plain(SigAlg),
		       signature = otp_to_plain(Signature)};

otp_to_plain(#'TBSCertificate'{signature = Signature,
			       issuer = Issuer,
			       subject = Subject,
			       subjectPublicKeyInfo = SPubKeyInfo,
			       extensions = Extensions} = TBSCert) ->
    
    TBSCert#'TBSCertificate'{signature = otp_to_plain(Signature),
			     issuer = otp_to_plain(Issuer),
			     subject = 
			     otp_to_plain(Subject),
			     subjectPublicKeyInfo = 
			     otp_to_plain(SPubKeyInfo),
			     extensions = otp_to_plain_extensions(Extensions)
			    };

otp_to_plain(#'SignatureAlgorithm'{} = SignAlg) ->
    {ok, EncSignAlg} = 'OTP-PUB-KEY':encode('SignatureAlgorithm', SignAlg),
    {ok, #'SignatureAlgorithm-Any'{algorithm = Algorithm, 
				   parameters = Params}} = 
	'OTP-PUB-KEY':decode('SignatureAlgorithm-Any', 
			     list_to_binary(EncSignAlg)),
    #'AlgorithmIdentifier'{algorithm = Algorithm, 
			   parameters = Params};

otp_to_plain({rdnSequence, SeqList}) when is_list(SeqList) ->
    {rdnSequence, 
     lists:map(fun(Seq) -> 
		       lists:map(fun(Element) -> 
					 otp_to_plain(Element)
				 end,
				 Seq)
	       end, SeqList)};

otp_to_plain(#'AttributeTypeAndValue'{type = Type, value = Value}) ->
    {ok, ATAVEnc} = 
	'OTP-PUB-KEY':encode('OTPAttributeTypeAndValue', 
			     #'OTPAttributeTypeAndValue'{type = Type, 
							 value = Value}),
    {ok, ATAVDec} = 'OTP-PUB-KEY':decode('AttributeTypeAndValue', 
					 list_to_binary(ATAVEnc)),
    ATAVDec;

otp_to_plain(#'SubjectPublicKeyInfo'{algorithm = 
				     #'SubjectPublicKeyInfoAlgorithm'{
				       algorithm = Algo,
				       parameters = 
				       Params},
				     subjectPublicKey = PublicKey}) ->
    
    OtpAlgo = #'OTPOLDSubjectPublicKeyInfo_algorithm'{algo = Algo,
						   parameters = Params},
    OtpDec = #'OTPOLDSubjectPublicKeyInfo'{algorithm = OtpAlgo,
					    subjectPublicKey = PublicKey},
    {ok, OtpEnc} = 
	'OTP-PUB-KEY':encode('OTPOLDSubjectPublicKeyInfo', OtpDec),
    
    {ok, AnyDec} = 'OTP-PUB-KEY':decode('OTPSubjectPublicKeyInfo-Any', 
					list_to_binary(OtpEnc)),
    
    #'OTPSubjectPublicKeyInfo-Any'{algorithm = #'PublicKeyAlgorithm'{
				     algorithm = NewAlgo,
				     parameters = NewParams},
				   subjectPublicKey = Bin} = AnyDec,
    
    #'SubjectPublicKeyInfo'{algorithm = 
			    #'AlgorithmIdentifier'{
			      algorithm = NewAlgo,
			      parameters = plain_key_params(NewParams)}, 
			    subjectPublicKey = 
			    {0, Bin}
			   };

otp_to_plain(#'Extension'{extnID = ExtID, 
			  extnValue = Value} = Ext) ->
    ExtValue = 
	otp_to_plain_extension_value(ExtID, Value),
    
    Ext#'Extension'{extnValue = ExtValue};

otp_to_plain(#'AuthorityKeyIdentifier'{} = Ext) ->
    CertIssuer = Ext#'AuthorityKeyIdentifier'.authorityCertIssuer,
    Ext#'AuthorityKeyIdentifier'{authorityCertIssuer = 
				 otp_to_plain(CertIssuer)};

otp_to_plain([{directoryName, Value}]) ->
    [{directoryName, otp_to_plain(Value)}];

otp_to_plain(Value) ->
    Value.

plain_key_params('NULL') ->
   <<5,0>>;
plain_key_params(Value) ->
    Value.

plain_to_otp_extension_value(?'id-ce-authorityKeyIdentifier', Value) ->
    plain_to_otp(Value);
plain_to_otp_extension_value(_, Value) ->
    Value.

plain_to_otp_extensions(Exts) when is_list(Exts) ->
    lists:map(fun(Ext) -> plain_to_otp(Ext) end, Exts).

otp_to_plain_extension_value(?'id-ce-authorityKeyIdentifier', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('AuthorityKeyIdentifier',
				     otp_to_plain(Value)),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-subjectKeyIdentifier', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('SubjectKeyIdentifier', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-keyUsage', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('KeyUsage', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-privateKeyUsagePeriod', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('PrivateKeyUsagePeriod', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-certificatePolicies', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('CertificatePolicies', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-policyMappings', Value) ->
    {ok, Enc} = 'OTP-PUB-KEY':encode('PolicyMappings', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-subjectAltName', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('SubjectAltName', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-issuerAltName', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('IssuerAltName', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-subjectDirectoryAttributes', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('SubjectDirectoryAttributes', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-basicConstraints', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('BasicConstraints', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-nameConstraints', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('NameConstraints', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-policyConstraints', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('PolicyConstraints', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-extKeyUsage', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('ExtKeyUsage', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-cRLDistributionPoints', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('CRLDistributionPoints', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-inhibitAnyPolicy', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('InhibitAnyPolicy', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(?'id-ce-freshestCRL', Value) ->
 {ok, Enc} = 'OTP-PUB-KEY':encode('FreshestCRL', Value),
    otp_to_plain_extension_value_format(Enc);
otp_to_plain_extension_value(_Id, Value) ->
    Value.

otp_to_plain_extension_value_format(Value) -> 
    list_to_binary(Value).

otp_to_plain_extensions(Exts) when is_list(Exts) ->
    lists:map(fun(Ext) ->
		      otp_to_plain(Ext)
	      end, Exts).
