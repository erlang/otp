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

-module(pubkey_cert_records).

-include("public_key.hrl").

-export([decode_cert/1, transform/2, supportedPublicKeyAlgorithms/1]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec decode_cert(DerCert::binary()) -> {ok, #'OTPCertificate'{}}.
%%
%% Description: Recursively decodes a Certificate. 
%%-------------------------------------------------------------------- 
decode_cert(DerCert) ->
    {ok, Cert} = 'OTP-PUB-KEY':decode('OTPCertificate', DerCert),
    #'OTPCertificate'{tbsCertificate = TBS} = Cert,
    {ok, Cert#'OTPCertificate'{tbsCertificate = decode_tbs(TBS)}}.

%%--------------------------------------------------------------------
-spec transform(term(), encode | decode) ->term().
%%
%% Description: Transforms between encoded and decode otp formated
%% certificate parts.
%%-------------------------------------------------------------------- 

transform(#'OTPCertificate'{tbsCertificate = TBS} = Cert, encode) ->
    Cert#'OTPCertificate'{tbsCertificate=encode_tbs(TBS)};
transform(#'OTPCertificate'{tbsCertificate = TBS} = Cert, decode) ->
    Cert#'OTPCertificate'{tbsCertificate=decode_tbs(TBS)};
transform(#'OTPTBSCertificate'{}= TBS, encode) ->
    encode_tbs(TBS);
transform(#'OTPTBSCertificate'{}= TBS, decode) ->
    decode_tbs(TBS);
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
transform(#'NameConstraints'{permittedSubtrees=Permitted, excludedSubtrees=Excluded}, Func) ->
    #'NameConstraints'{permittedSubtrees=transform_sub_tree(Permitted,Func),
		       excludedSubtrees=transform_sub_tree(Excluded,Func)};
	  
transform(Other,_) ->
    Other.

%%--------------------------------------------------------------------
-spec supportedPublicKeyAlgorithms(Oid::tuple()) -> asn1_type().
%%
%% Description: Returns the public key type for an algorithm
%% identifier tuple as found in SubjectPublicKeyInfo.
%%
%%--------------------------------------------------------------------
supportedPublicKeyAlgorithms(?'rsaEncryption') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-dsa') -> 'DSAPublicKey';
supportedPublicKeyAlgorithms(?'dhpublicnumber') -> 'DHPublicKey';
supportedPublicKeyAlgorithms(?'id-keyExchangeAlgorithm') -> 'KEA-PublicKey';
supportedPublicKeyAlgorithms(?'id-ecPublicKey') -> 'ECPoint'.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%% SubjectPublicKey

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

encode_tbs(TBS=#'OTPTBSCertificate'{issuer=Issuer0,
				    subject=Subject0,
				    subjectPublicKeyInfo=Spki0,
				    extensions=Exts0}) ->
    Issuer  = transform(Issuer0,encode),
    Subject = transform(Subject0,encode),
    Spki = encode_supportedPublicKey(Spki0),
    Exts = encode_extensions(Exts0),
    TBS#'OTPTBSCertificate'{issuer=Issuer, subject=Subject,
			    subjectPublicKeyInfo=Spki,extensions=Exts}.

decode_tbs(TBS = #'OTPTBSCertificate'{issuer=Issuer0,
				      subject=Subject0,
				      subjectPublicKeyInfo=Spki0,
				      extensions=Exts0}) -> 
    Issuer  = transform(Issuer0,decode),
    Subject = transform(Subject0,decode),
    Spki = decode_supportedPublicKey(Spki0),
    Exts = decode_extensions(Exts0),
    TBS#'OTPTBSCertificate'{issuer=Issuer, subject=Subject,
			    subjectPublicKeyInfo=Spki,extensions=Exts}.

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
