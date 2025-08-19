
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

-ifndef(public_key).
-define(public_key, true).

%%%
%%% RSA PKCS-1 & PSS-OAEP
%%%

-define('pkcs-1', {1,2,840,113549,1,1}).

-record('RSAPublicKey',
        {
         modulus,
         publicExponent
        }).

-record('RSAPrivateKey',
        {
         version,
         modulus,
         publicExponent,
         privateExponent,
         prime1,
         prime2,
         exponent1,
         exponent2,
         coefficient,
         otherPrimeInfos = asn1_NOVALUE
  }).

-record('OtherPrimeInfo',
        {
         prime,
         exponent,
         coefficient
        }).

-record('RSASSA-PSS-params',
        {
         hashAlgorithm = asn1_DEFAULT,
         maskGenAlgorithm = asn1_DEFAULT,
         saltLength = asn1_DEFAULT,
         trailerField = asn1_DEFAULT
        }).

-record('RSASSA-AlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-define('id-RSAES-OAEP', {1,2,840,113549,1,1,7}).

-record('RSAES-OAEP-params',
        {
         hashAlgorithm = asn1_DEFAULT,
         maskGenAlgorithm = asn1_DEFAULT,
         pSourceAlgorithm = asn1_DEFAULT
        }).

-record('RSAES-AlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).


-record('HashAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('MaskGenAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PSourceAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-define('id-pSpecified', {1,2,840,113549,1,1,9}).
-define('pSpecifiedEmpty', {'PSourceAlgorithm',{1,2,840,113549,1,1,9},<<>>}).
-define('emptyString', <<>>).
-define('nullOctetString', <<>>).
-define('nullParameters', 'NULL').


%%%
%%% ML-DSA
%%%
-record('ML-DSAPrivateKey',
        {
         algorithm :: mldsa44 | mldsa65 | mldsa87,
         seed = <<>>   :: binary(),
         expandedkey = <<>> :: binary()
        }).

-record('ML-DSAPublicKey',
        {
         algorithm :: mldsa44 | mldsa65 | mldsa87,
         key  :: binary()
        }).

-define('id-ml-dsa-44', {2,16,840,1,101,3,4,3,17}).
-define('id-ml-dsa-65', {2,16,840,1,101,3,4,3,18}).
-define('id-ml-dsa-87', {2,16,840,1,101,3,4,3,19}).


%%%
%%% DSA
%%%

-record('DSAPrivateKey',
        {
         version,      % pos_integer()
         p,            % pos_integer()
         q,            % pos_integer()
         g,            % pos_integer()
         y,            % pos_integer()
         x             % pos_integer()
        }).

-record('Dss-Parms',
        {
         p,         % pos_integer()
         q,         % pos_integer()
         g          % pos_integer()
        }).

-record('DomainParameters',
        {
         p,
         g,
         q,
         j = asn1_NOVALUE,
         validationParms = asn1_NOVALUE
        }).

-record('ValidationParams',
        {
         seed,
         pgenCounter
        }).


-record('Dss-Sig-Value',
        {
         r,
         s
        }).

%%%
%%% ECDSA, EDDSA, ECDH(E)
%%%
-define('id-edwards-curve-algs', {1,3,101}).

-define('id-Ed25519', {1,3,101,112}).
-define('id-Ed448', {1,3,101,113}).

-define('id-X25519', {1,3,101,110}).
-define('id-X448', {1,3,101,111}).

%% ECC
-define('secp192r1', {1,2,840,10045,3,1,1}).
-define('sect163k1', {1,3,132,0,1}).
-define('sect163r2', {1,3,132,0,15}).
-define('secp224r1', {1,3,132,0,33}).
-define('sect233k1', {1,3,132,0,26}).
-define('sect233r1', {1,3,132,0,27}).
-define('secp256r1', {1,2,840,10045,3,1,7}).
-define('sect283k1', {1,3,132,0,16}).
-define('sect283r1', {1,3,132,0,17}).
-define('secp384r1', {1,3,132,0,34}).
-define('sect409k1', {1,3,132,0,36}).
-define('sect409r1', {1,3,132,0,37}).
-define('secp521r1', {1,3,132,0,35}).
-define('sect571k1', {1,3,132,0,38}).
-define('sect571r1', {1,3,132,0,39}).

%% Legacy
-define('secp224k1', {1,3,132,0,32}).
-define('secp192k1', {1,3,132,0,31}).
-define('secp160r2', {1,3,132,0,30}).
-define('secp128r2', {1,3,132,0,29}).
-define('secp128r1', {1,3,132,0,28}).
-define('sect193r2', {1,3,132,0,25}).
-define('sect193r1', {1,3,132,0,24}).
-define('sect131r2', {1,3,132,0,23}).
-define('sect131r1', {1,3,132,0,22}).
-define('secp256k1', {1,3,132,0,10}).
-define('secp160k1', {1,3,132,0,9}).
-define('secp160r1', {1,3,132,0,8}).
-define('secp112r2', {1,3,132,0,7}).
-define('secp112r1', {1,3,132,0,6}).
-define('sect113r2', {1,3,132,0,5}).
-define('sect113r1', {1,3,132,0,4}).
-define('sect239k1', {1,3,132,0,3}).
-define('sect163r1', {1,3,132,0,2}).

%% Brainpool
-define('ellipticCurveRFC5639', {1,3,36,3,3,2,8,1}).
-define('versionOne', {1,3,36,3,3,2,8,1,1}).
-define('ecStdCurvesAndGeneration', {1,3,36,3,3,2,8}).

-define('brainpoolP160r1', {1,3,36,3,3,2,8,1,1,1}).
-define('brainpoolP160t1', {1,3,36,3,3,2,8,1,1,2}).
-define('brainpoolP192r1', {1,3,36,3,3,2,8,1,1,3}).
-define('brainpoolP192t1', {1,3,36,3,3,2,8,1,1,4}).
-define('brainpoolP224r1', {1,3,36,3,3,2,8,1,1,5}).
-define('brainpoolP224t1', {1,3,36,3,3,2,8,1,1,6}).
-define('brainpoolP256r1', {1,3,36,3,3,2,8,1,1,7}).
-define('brainpoolP256t1', {1,3,36,3,3,2,8,1,1,8}).
-define('brainpoolP320r1', {1,3,36,3,3,2,8,1,1,9}).
-define('brainpoolP320t1', {1,3,36,3,3,2,8,1,1,10}).
-define('brainpoolP384r1', {1,3,36,3,3,2,8,1,1,11}).
-define('brainpoolP384t1', {1,3,36,3,3,2,8,1,1,12}).
-define('brainpoolP512r1', {1,3,36,3,3,2,8,1,1,13}).
-define('brainpoolP512t1', {1,3,36,3,3,2,8,1,1,14}).

-record('ECPrivateKey',
        {
         version,
         privateKey,
         parameters = asn1_NOVALUE,
         publicKey = asn1_NOVALUE,
         attributes = asn1_NOVALUE
        }).

-record('ECParameters',
        {
         version,
         fieldID,
         curve,
         base,
         order,
         cofactor = asn1_NOVALUE
        }).


-record('Curve',
        {
         a,
         b,
         seed = asn1_NOVALUE
        }).

-record('FieldID',
        {
         fieldType,
         parameters
        }).

-record('ECPoint',
        {
         point
        }).

-record('ECDSA-Sig-Value',
        {
         r,
         s
        }).

%%%
%%% PKIX Certificates
%%%
%% plain certificate format
-record('Certificate',
        {
         tbsCertificate,
         signatureAlgorithm,
         signature
        }).

-record('TBSCertificate',
        {
         version = asn1_DEFAULT,
         serialNumber,
         signature,
         issuer,
         validity,
         subject,
         subjectPublicKeyInfo,
         issuerUniqueID = asn1_NOVALUE,
         subjectUniqueID = asn1_NOVALUE,
         extensions = asn1_NOVALUE
        }).

-record('TBSCertificate_signature',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-define('id-at', {2,5,4}).
-define('id-at-name', {2,5,4,41}).
-define('id-at-surname', {2,5,4,4}).
-define('id-at-givenName', {2,5,4,42}).
-define('id-at-initials', {2,5,4,43}).
-define('id-at-generationQualifier', {2,5,4,44}).
-define('id-at-commonName', {2,5,4,3}).
-define('id-at-localityName', {2,5,4,7}).
-define('id-at-stateOrProvinceName', {2,5,4,8}).
-define('id-at-organizationName', {2,5,4,10}).
-define('id-at-title', {2,5,4,12}).
-define('id-at-dnQualifier', {2,5,4,46}).
-define('id-at-countryName', {2,5,4,6}).
-define('id-at-serialNumber', {2,5,4,5}).
-define('id-at-pseudonym', {2,5,4,65}).

%% Should we document ?
-define('id-domainComponent', {0,9,2342,19200300,100,1,25}).
-define('id-emailAddress', {1,2,840,113549,1,9,1}).
-define('id-at-organizationalUnitName', {2,5,4,11}).

-record('Validity',
        {
         notBefore,
         notAfter
        }).

-record('SubjectPublicKeyInfo',
        {
         algorithm,
         subjectPublicKey
        }).


%% plain certificate format used in SubjectPublicKeyInfo
-record('AlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).


-record('AttributeTypeAndValue',
        {
         type,
         value
        }).

%% PKIX Common Type
-record('Extension',
        {
         extnID,
         critical = asn1_DEFAULT,
         extnValue
        }).

-record('AttributeSet',
        {
         type,
         values
        }).

-record('SingleAttribute',
        {
         type,
         value
        }).

-record('SecurityCategory',
        {
         type,
         value
        }).

%%%
%%% Standard Certificate Extensions
%%%
-define('id-ce', {2,5,29}).
-define('id-ce-targetInformation', {2,5,29,55}).
-define('id-ce-invalidityDate', {2,5,29,24}).
-define('id-ce-holdInstructionCode', {2,5,29,23}).
-define('id-ce-certificateIssuer', {2,5,29,29}).
-define('id-ce-cRLReasons', {2,5,29,21}).
-define('id-ce-deltaCRLIndicator', {2,5,29,27}).
-define('id-ce-issuingDistributionPoint', {2,5,29,28}).
-define('id-ce-cRLNumber', {2,5,29,20}).
-define('id-ce-freshestCRL', {2,5,29,46}).
-define('id-ce-inhibitAnyPolicy', {2,5,29,54}).
-define('id-ce-extKeyUsage', {2,5,29,37}).
-define('id-ce-cRLDistributionPoints', {2,5,29,31}).
-define('id-ce-policyConstraints', {2,5,29,36}).
-define('id-ce-nameConstraints', {2,5,29,30}).
-define('id-ce-basicConstraints', {2,5,29,19}).
-define('id-ce-subjectDirectoryAttributes', {2,5,29,9}).
-define('id-ce-issuerAltName', {2,5,29,18}).
-define('id-ce-subjectAltName', {2,5,29,17}).
-define('id-ce-policyMappings', {2,5,29,33}).
-define('id-ce-certificatePolicies', {2,5,29,32}).
-define('id-ce-privateKeyUsagePeriod', {2,5,29,16}).
-define('id-ce-keyUsage', {2,5,29,15}).
-define('id-ce-subjectKeyIdentifier', {2,5,29,14}).
-define('id-ce-authorityKeyIdentifier', {2,5,29,35}).

-define('id-ad-caIssuers', {1,3,6,1,5,5,7,48,2}).
-define('id-ad-timeStamping', {1,3,6,1,5,5,7,48,3}).
-define('id-ad-caRepository', {1,3,6,1,5,5,7,48,5}).

-define('id-pe-authorityInfoAccess', {1,3,6,1,5,5,7,1,1}).
-define('id-pe-subjectInfoAccess', {1,3,6,1,5,5,7,1,11}).

-define('anyExtendedKeyUsage', {2,5,29,37,0}).
-define('anyPolicy', {2,5,29,32,0}).

-define('id-pkix', {1,3,6,1,5,5,7}).

-define('id-kp', {1,3,6,1,5,5,7,3}).
-define('id-kp-timeStamping', {1,3,6,1,5,5,7,3,8}).
-define('id-kp-emailProtection', {1,3,6,1,5,5,7,3,4}).
-define('id-kp-codeSigning', {1,3,6,1,5,5,7,3,3}).
-define('id-kp-clientAuth', {1,3,6,1,5,5,7,3,2}).
-define('id-kp-serverAuth', {1,3,6,1,5,5,7,3,1}).

-define('id-qt', {1,3,6,1,5,5,7,2}).
-define('id-qt-cps', {1,3,6,1,5,5,7,2,1}).
-define('id-qt-unotice', {1,3,6,1,5,5,7,2,2}).

-define('holdInstruction', {2,2,840,10040,2}).
-define('id-holdinstruction-none', {2,2,840,10040,2,1}).
-define('id-holdinstruction-callissuer', {2,2,840,10040,2,2}).
-define('id-holdinstruction-reject', {2,2,840,10040,2,3}).

-record('AuthorityKeyIdentifier',
        {
         keyIdentifier = asn1_NOVALUE,
         authorityCertIssuer = asn1_NOVALUE,
         authorityCertSerialNumber = asn1_NOVALUE
        }).

-record('PrivateKeyUsagePeriod',
        {
         notBefore = asn1_NOVALUE,
         notAfter = asn1_NOVALUE
        }).

-record('PolicyInformation',
        {
         policyIdentifier,
         policyQualifiers = asn1_NOVALUE
        }).

-record('PolicyQualifierInfo',
        {
         policyQualifierId,
         qualifier
        }).

-record('UserNotice',
        {
         noticeRef = asn1_NOVALUE,
         explicitText = asn1_NOVALUE
        }).

-record('NoticeReference',
        {
         organization,
         noticeNumbers
        }).

-record('PolicyMappings_SEQOF',
        {
         issuerDomainPolicy,
         subjectDomainPolicy
        }).

-record('EDIPartyName',
        {
         nameAssigner = asn1_NOVALUE,
         partyName
        }).

-record('SubjectDirectoryAttributes_SEQOF',
        {
         type,
         values
        }).

-record('BasicConstraints',
        {
         cA = asn1_DEFAULT,
         pathLenConstraint = asn1_NOVALUE
        }).

-record('NameConstraints',
        {
         permittedSubtrees = asn1_NOVALUE,
         excludedSubtrees = asn1_NOVALUE
        }).

-record('GeneralSubtree',
        {
         base,
         minimum = asn1_DEFAULT,
         maximum = asn1_NOVALUE
        }).

-record('PolicyConstraints',
        {
         requireExplicitPolicy = asn1_NOVALUE,
         inhibitPolicyMapping = asn1_NOVALUE
        }).

-record('DistributionPoint',
        {
         distributionPoint = asn1_NOVALUE,
         reasons = asn1_NOVALUE,
         cRLIssuer = asn1_NOVALUE
        }).

-record('AccessDescription',
        {
         accessMethod,
         accessLocation
        }).

%%%
%%% Erlang alternate representation of PKIX certificate
%%%

-record('OTPCertificate',
        {
         tbsCertificate,
         signatureAlgorithm,
         signature
        }).

-record('OTPTBSCertificate',
        {
         version = asn1_DEFAULT,
         serialNumber,
         signature,
         issuer,
         validity,
         subject,
         subjectPublicKeyInfo,
         issuerUniqueID = asn1_NOVALUE,
         subjectUniqueID = asn1_NOVALUE,
         extensions = asn1_NOVALUE
        }).

%% backwards compatibility
-record('OTPSubjectPublicKeyInfo',
        {
         algorithm,
         subjectPublicKey
        }).

-record('SignatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

%% OTP certificate format used in SubjectPublicKeyInfo
-record('PublicKeyAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record(cert,
        {
         der :: public_key:der_encoded(),
         otp :: #'OTPCertificate'{}
        }).

%% Hash functions
-record('DigestAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-define('id-sha224', {2,16,840,1,101,3,4,2,4}).
-define('id-sha256', {2,16,840,1,101,3,4,2,1}).
-define('id-sha384', {2,16,840,1,101,3,4,2,2}).
-define('id-sha512', {2,16,840,1,101,3,4,2,3}).

%% Legacy hash functions
-define('id-sha1', {1,3,14,3,2,26}).
-define('id-md2', {1,2,840,113549,2,2}).
-define('id-md5', {1,2,840,113549,2,5}).

%%%
%%% Public-key algorithms
%%%

%% Digital signatures
%% Modern
-define('id-RSASSA-PSS', {1,2,840,113549,1,1,10}).
-define('rSASSA-PSS-Default-Identifier', 
        {'RSASSA-AlgorithmIdentifier',{1,2,840,113549,1,1,10},
         {'RSASSA-PSS-params',{'HashAlgorithm',{1,3,14,3,2,26},'NULL'},
          {'MaskGenAlgorithm',{1,2,840,113549,1,1,8},
           {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}},20,1}}).
-define('rSAES-OAEP-Default-Identifier', {'RSAES-AlgorithmIdentifier',{1,2,840,113549,1,1,7},
                                          {'RSAES-OAEP-params',{'Externalvaluereference',354,'PKCS-1',sha1},
                                           {'Externalvaluereference',355,'PKCS-1',mgf1SHA1},
                                           {'Externalvaluereference',356,'PKCS-1',pSpecifiedEmpty}}}).
-define('id-mgf1', {1,2,840,113549,1,1,8}).
-define('sha1Identifier', {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}).
-define('sha1', {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}).
-define('mgf1SHA1', {'MaskGenAlgorithm',{1,2,840,113549,1,1,8},
                     {'Externalvaluereference',283,'PKIX1-PSS-OAEP-Algorithms-2009',sha1Identifier}}).
-define('id-ecPublicKey', {1,2,840,10045,2,1}).
-define('ecdsa-with-SHA224', {1,2,840,10045,4,3,1}).
-define('ecdsa-with-SHA256', {1,2,840,10045,4,3,2}).
-define('ecdsa-with-SHA384', {1,2,840,10045,4,3,3}).
-define('ecdsa-with-SHA512', {1,2,840,10045,4,3,4}).

%% Legacy
-define('ecdsa-with-SHA2', {1,2,840,10045,4,3}).
-define('rsaEncryption', {1,2,840,113549,1,1,1}).
-define('md2WithRSAEncryption', {1,2,840,113549,1,1,2}).
-define('md5WithRSAEncryption', {1,2,840,113549,1,1,4}).
-define('sha1WithRSAEncryption', {1,2,840,113549,1,1,5}).
-define('sha224WithRSAEncryption', {1,2,840,113549,1,1,14}).
-define('sha256WithRSAEncryption', {1,2,840,113549,1,1,11}).
-define('sha384WithRSAEncryption', {1,2,840,113549,1,1,12}).
-define('sha512WithRSAEncryption', {1,2,840,113549,1,1,13}).
-define('sha512-224WithRSAEncryption', {1,2,840,113549,1,1,15}).
-define('sha512-256WithRSAEncryption', {1,2,840,113549,1,1,16}).
-define('sha-1WithRSAEncryption', {1,3,14,3,2,29}).
-define('id-hmacWithSHA1', {1,2,840,113549,2,7}).
-define('ecdsa-with-SHA1', {1,2,840,10045,4,1}).
-define('id-dsa', {1,2,840,10040,4,1}).
-define('id-dsaWithSHA1', {1,3,14,3,2,27}).
-define('id-dsa-with-sha1', {1,2,840,10040,4,3}).
-define('id-dsa-with-sha224', {2,16,840,1,101,3,4,3,1}).
-define('id-dsa-with-sha256', {2,16,840,1,101,3,4,3,2}).

%% Key exchange
-define('dhpublicnumber', {1,2,840,10046,2,1}).
-define('id-keyExchangeAlgorithm', {2,16,840,1,101,2,1,1,22}).
-define('pkcs-3', {1,2,840,113549,1,3}).
-define('dhKeyAgreement', {1,2,840,113549,1,3,1}).

-record('DHParameter',
        {
         prime,
         base,
         privateValueLength = asn1_NOVALUE
        }).

%% From PKCS-9 (now part of CertificateEnrollmentMessageSyntax)
-define('pkcs-9-at-extensionRequest', {1,2,840,113549,1,9,14}).

%%% CRL and CRL Extensions Profile
%%%

-define(unspecified, 0).
-define(keyCompromise, 1).
-define(cACompromise, 2).
-define(affiliationChanged, 3).
-define(superseded, 4).
-define(cessationOfOperation, 5).
-define(certificateHold, 6).
-define(removeFromCRL, 8).
-define(privilegeWithdrawn, 9).
-define(aACompromise, 10).

-record('CertificateList',
        {
         tbsCertList,
         signatureAlgorithm,
         signature
        }).

-record('TBSCertList',
        {
         version = asn1_NOVALUE,
         signature,
         issuer,
         thisUpdate,
         nextUpdate = asn1_NOVALUE,
         revokedCertificates = asn1_NOVALUE,
         crlExtensions = asn1_NOVALUE
        }).

-record('TBSCertList_signature',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('CertificateList_algorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('TBSCertList_revokedCertificates_SEQOF',
        {
         userCertificate,
         revocationDate,
         crlEntryExtensions = asn1_NOVALUE
       }).

%%%
%%% CRL Extensions
%%%

-record('IssuingDistributionPoint',
        {
         distributionPoint = asn1_NOVALUE,
         onlyContainsUserCerts = asn1_DEFAULT,
         onlyContainsCACerts = asn1_DEFAULT,
         onlySomeReasons = asn1_NOVALUE,
         indirectCRL = asn1_DEFAULT,
         onlyContainsAttributeCerts = asn1_DEFAULT
        }).

%%%
%%% PKCS#10 Certification Request
%%%

-record('CertificationRequest',
        {
         certificationRequestInfo,
         signatureAlgorithm,
         signature
        }).

-record('CertificationRequestInfo',
        {
         version,
         subject,
         subjectPKInfo,
         attributes
        }).

-record('CertificationRequestInfo_subjectPKInfo',
        {
         algorithm,
         subjectPublicKey
        }).

-record('CertificationRequestInfo_subjectPKInfo_algorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).


-record('CertificationRequestInfo_attributes_SETOF',
        {
         type,
         values
        }).

-record('CertificationRequest_signatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

%%%
%%% OCSP
%%%
-define('id-kp-OCSPSigning', {1,3,6,1,5,5,7,3,9}).
-define('id-pkix-ocsp', {1,3,6,1,5,5,7,48,1}).
-define('id-pkix-ocsp-basic', {1,3,6,1,5,5,7,48,1,1}).
-define('id-pkix-ocsp-nonce', {1,3,6,1,5,5,7,48,1,2}).
-define('id-pkix-ocsp-crl', {1,3,6,1,5,5,7,48,1,3}).
-define('id-pkix-ocsp-response', {1,3,6,1,5,5,7,48,1,4}).
-define('id-pkix-ocsp-nocheck', {1,3,6,1,5,5,7,48,1,5}).
-define('id-pkix-ocsp-archive-cutoff', {1,3,6,1,5,5,7,48,1,6}).
-define('id-pkix-ocsp-service-locator', {1,3,6,1,5,5,7,48,1,7}).
-define('id-pkix-ocsp-pref-sig-algs', {1,3,6,1,5,5,7,48,1,8}).
-define('id-pkix-ocsp-extended-revoke', {1,3,6,1,5,5,7,48,1,9}).

-define('id-ad-ocsp', {1,3,6,1,5,5,7,48,1}).

-record('OCSPRequest',
       {
        tbsRequest,
        optionalSignature = asn1_NOVALUE
       }).

-record('TBSRequest',
        {
         version = asn1_DEFAULT,
         requestorName = asn1_NOVALUE,
         requestList,
         requestExtensions = asn1_NOVALUE
        }).

-record('TBSRequest_requestExtensions_SEQOF',
        {
         extnID,
         critical = asn1_DEFAULT,
         extnValue
        }).

-record('Signature',
        {
         signatureAlgorithm,
         signature,
         certs = asn1_NOVALUE
        }).

-record('Signature_signatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('Request',
        {
         reqCert,
         singleRequestExtensions = asn1_NOVALUE
        }).

-record('Request_singleRequestExtensions_SEQOF',
        {
         extnID,
         critical = asn1_DEFAULT,
         extnValue
        }).

-record('CertID',
        {
         hashAlgorithm,
         issuerNameHash,
         issuerKeyHash,
         serialNumber
        }).

-record('CertID_hashAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('OCSPResponse',
        {
         responseStatus,
         responseBytes = asn1_NOVALUE
        }).

-record('ResponseBytes',
        {
         responseType,
         response
        }).

-record('BasicOCSPResponse',
        {
         tbsResponseData,
         signatureAlgorithm,
         signature,
         certs = asn1_NOVALUE
        }).

-record('ResponseData',
        {
         version = asn1_DEFAULT,
         responderID,
         producedAt,
         responses,
         responseExtensions = asn1_NOVALUE
        }).

-record('BasicOCSPResponse_signatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('ResponseData_responseExtensions_SEQOF',
        {
         extnID,
         critical = asn1_DEFAULT,
         extnValue
        }).

-record('SingleResponse',
        {
         certID,
         certStatus,
         thisUpdate,
         nextUpdate = asn1_NOVALUE,
         singleExtensions = asn1_NOVALUE
        }).

-record('SingleResponse_singleExtensions_SEQOF',
        {
         extnID,
         critical = asn1_DEFAULT,
         extnValue
        }).

-record('RevokedInfo',
        {
         revocationTime,
         revocationReason = asn1_NOVALUE
        }).

-record('ServiceLocator',
        {
         issuer,
         locator
        }).

-record('CrlID',
        {
         crlUrl = asn1_NOVALUE,
         crlNum = asn1_NOVALUE,
         crlTime = asn1_NOVALUE
        }).

-record('PreferredSignatureAlgorithm',
        {
         sigIdentifier,
         certIdentifier = asn1_NOVALUE
        }).

-record('PreferredSignatureAlgorithm_sigIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PreferredSignatureAlgorithm_certIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).


%%%
%%% PKCS-8
%%%

-record('PrivateKeyInfo',
        {   %% OneAsymmetricKey
            version,
            privateKeyAlgorithm,
            privateKey,
            attributes = asn1_NOVALUE,
            %% Version 2 with extension
            publicKey = asn1_NOVALUE
        }).

-record('PrivateKeyInfo_privateKeyAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('EncryptedPrivateKeyInfo',
        {
         encryptionAlgorithm,
         encryptedData
        }).

-record('EncryptedPrivateKeyInfo_encryptionAlgorithm',
        {algorithm,
         parameters
        }).

-record('OneAsymmetricKey',
        {
         version,
         privateKeyAlgorithm,
         privateKey,
         attributes = asn1_NOVALUE,
         %% with extensions
         publicKey = asn1_NOVALUE
         %% end of extensions
        }).

%%%
%%% Password based encryption
%%%

-define('pkcs-5', {1,2,840,113549,1,5}).

-define('pbeWithSHA1AndRC2-CBC', {1,2,840,113549,1,5,11}).
-define('pbeWithSHA1AndDES-CBC', {1,2,840,113549,1,5,10}).
-define('pbeWithMD5AndRC2-CBC', {1,2,840,113549,1,5,6}).
-define('pbeWithMD5AndDES-CBC', {1,2,840,113549,1,5,3}).
-define('pbeWithMD2AndRC2-CBC', {1,2,840,113549,1,5,4}).
-define('pbeWithMD2AndDES-CBC', {1,2,840,113549,1,5,1}).

-define('id-PBES2', {1,2,840,113549,1,5,13}).
-define('id-PBKDF2', {1,2,840,113549,1,5,12}).
-define('defaultPBKDF2', {'PBKDF2-PRFsAlgorithmIdentifier',{1,3,6,1,5,5,8,1,2},'NULL'}).

-define('id-hmacWithSHA224', {1,2,840,113549,2,8}).
-define('id-hmacWithSHA256', {1,2,840,113549,2,9}).
-define('id-hmacWithSHA384', {1,2,840,113549,2,10}).
-define('id-hmacWithSHA512', {1,2,840,113549,2,11}).

-define('aes', {2,16,840,1,101,3,4,1}).
-define('id-aes256-wrap', {2,16,840,1,101,3,4,1,45}).
-define('id-aes192-wrap', {2,16,840,1,101,3,4,1,25}).
-define('id-aes128-wrap', {2,16,840,1,101,3,4,1,5}).
-define('id-aes128-CBC', {2,16,840,1,101,3,4,1,2}).
-define('id-aes192-CBC', {2,16,840,1,101,3,4,1,22}).
-define('id-aes256-CBC', {2,16,840,1,101,3,4,1,42}).
-define('rc2CBC', {1,2,840,113549,3,2}).
-define('des-EDE3-CBC', {1,2,840,113549,3,7}).
-define('desCBC', {1,3,14,3,2,7}).

-record('RC2-CBC-Parameter',
        {
         rc2ParameterVersion = asn1_NOVALUE,
         iv
        }).

-record('PBES2-params',
        {
         keyDerivationFunc,
         encryptionScheme
        }).

-record('PBES2-params_keyDerivationFunc',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PBES2-params_encryptionScheme',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PBEParameter',
        {
         salt,
         iterationCount
        }).

-record('PBKDF2-params',
        {
         salt,
         iterationCount,
         keyLength = asn1_NOVALUE,
         prf = asn1_DEFAULT
        }).

-record('PBKDF2-params_prf',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('EncryptionAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).
%%%
%%% CryptographicMessageSyntax
%%%

-define('id-ct-contentInfo', {1,2,840,113549,1,9,16,1,6}).
-define('id-data', {1,2,840,113549,1,7,1}).
-define('id-signedData', {1,2,840,113549,1,7,2}).
-define('id-envelopedData', {1,2,840,113549,1,7,3}).
-define('id-digestedData', {1,2,840,113549,1,7,5}).
-define('id-encryptedData', {1,2,840,113549,1,7,6}).
-define('id-ct-authData', {1,2,840,113549,1,9,16,1,2}).
-define('id-contentType', {1,2,840,113549,1,9,3}).
-define('id-messageDigest', {1,2,840,113549,1,9,4}).
-define('id-signingTime', {1,2,840,113549,1,9,5}).
-define('id-countersignature', {1,2,840,113549,1,9,6}).
-define('des-ede3-cbc', {1,2,840,113549,3,7}).
-define('rc2-cbc', {1,2,840,113549,3,2}).

%% Legacy names for backwards compatibility
-define('encryptedData', {1,2,840,113549,1,7,6}).
-define('digestedData', {1,2,840,113549,1,7,5}).
-define('envelopedData', {1,2,840,113549,1,7,3}).
-define('signedData', {1,2,840,113549,1,7,2}).
-define('data', {1,2,840,113549,1,7,1}).

-record('Attribute',
        {
         type,
         values
        }).

-record('ContentInfo',
        {
         contentType,
         content
        }).

-record('SignedData',
        {
         version,
         digestAlgorithms,
         encapContentInfo,
         certificates = asn1_NOVALUE,
         crls = asn1_NOVALUE,
         signerInfos
        }).

-record('EncapsulatedContentInfo',
        {
         eContentType,
         eContent = asn1_NOVALUE
        }).

-record('SignerInfo',
        {
         version,
         sid,
         digestAlgorithm,
         signedAttrs = asn1_NOVALUE,
         signatureAlgorithm,
         signature,
         unsignedAttrs = asn1_NOVALUE
        }).

-record('SignerInfo_unsignedAttrs_SETOF',
        {
         attrType,
         attrValues
        }).

-record('SignedAttributes_SETOF',
        {
         attrType,
         attrValues
        }).

-record('EnvelopedData',
        {
         version,
         originatorInfo = asn1_NOVALUE,
         recipientInfos,
         encryptedContentInfo,
         %% with extensions
         unprotectedAttrs = asn1_NOVALUE
        }).

-record('EnvelopedData_unprotectedAttrs_SETOF',
        {
         attrType,
         attrValues
        }).

-record('OriginatorInfo',
        {
         certs = asn1_NOVALUE,
         crls = asn1_NOVALUE
        }).

-record('EncryptedContentInfo',
        {
         contentType,
         contentEncryptionAlgorithm,
         encryptedContent = asn1_NOVALUE
        }).

-record('KeyTransRecipientInfo',
        {
         version,
         rid,
         keyEncryptionAlgorithm,
         encryptedKey
        }).

-record('KeyTransRecipientInfo_keyEncryptionAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('KeyAgreeRecipientInfo',
        {
         version,
         originator,
         ukm = asn1_NOVALUE,
         keyEncryptionAlgorithm,
         recipientEncryptedKeys
        }).

-record('KeyAgreeRecipientInfo_keyEncryptionAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('OriginatorPublicKey',
        {
         algorithm,
         publicKey
        }).

-record('OriginatorPublicKey_algorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('RecipientEncryptedKey',
        {
         rid,
         encryptedKey
        }).

-record('RecipientKeyIdentifier',
        {
         subjectKeyIdentifier,
         date = asn1_NOVALUE,
         other = asn1_NOVALUE
        }).

-record('KEKRecipientInfo',
        {
         version,
         kekid,
         keyEncryptionAlgorithm,
         encryptedKey
        }).

-record('KEKIdentifier',
        {
         keyIdentifier,
         date = asn1_NOVALUE,
         other = asn1_NOVALUE
        }).

-record('PasswordRecipientInfo',
        {
         version,
         keyDerivationAlgorithm = asn1_NOVALUE,
         keyEncryptionAlgorithm,
         encryptedKey
        }).

-record('OtherRecipientInfo',
        {
         oriType,
         oriValue
        }).

-record('DigestedData',
        {
         version,
         digestAlgorithm,
         encapContentInfo,
         digest
         %% with extension mark
        }).

-record('EncryptedData',
        {
         version,
         encryptedContentInfo,
         %% with extensions
         unprotectedAttrs = asn1_NOVALUE
        }).

-record('EncryptedData_unprotectedAttrs_SETOF',
        {
         attrType,
         attrValues
        }).

-record('AuthenticatedData',
        {
         version,
         originatorInfo = asn1_NOVALUE,
         recipientInfos,
         macAlgorithm,
         digestAlgorithm = asn1_NOVALUE,
         encapContentInfo,
         authAttrs = asn1_NOVALUE,
         mac,
         unauthAttrs = asn1_NOVALUE
        }).

-record('AuthAttributes_SETOF',
        {
         attrType,
         attrValues
        }).

-record('UnauthAttributes_SETOF',
        {
         attrType,
         attrValues
        }).

-record('DigestAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('SignatureAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('KeyEncryptionAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('ContentEncryptionAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('MessageAuthenticationCodeAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('KeyDerivationAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('OtherRevocationInfoFormat',
        {
         otherRevInfoFormat,
         otherRevInfo
        }).

-record('OtherCertificateFormat',
        {
         otherCertFormat,
         otherCert
}).

-record('IssuerAndSerialNumber',
        {
         issuer,
         serialNumber
        }).

-record('OtherKeyAttribute',
        {
         keyAttrId,
         keyAttr
        }).

-record('ExtendedCertificate',
        {
         extendedCertificateInfo,
         signatureAlgorithm,
         signature
        }).

-record('ExtendedCertificateInfo',
        {
         version,
         certificate,
         attributes
        }).


%% X400 addresses
-record('ORAddress',
        {
         'built-in-standard-attributes',
         'built-in-domain-defined-attributes' = asn1_NOVALUE,
         'extension-attributes' = asn1_NOVALUE
        }).

-record('BuiltInStandardAttributes',
        {
         'country-name' = asn1_NOVALUE,
         'administration-domain-name' = asn1_NOVALUE,
         'network-address' = asn1_NOVALUE,
         'terminal-identifier' = asn1_NOVALUE,
         'private-domain-name' = asn1_NOVALUE,
         'organization-name' = asn1_NOVALUE,
         'numeric-user-identifier' = asn1_NOVALUE,
         'personal-name' = asn1_NOVALUE,
         'organizational-unit-names' = asn1_NOVALUE
        }).

-record('PersonalName',
        {
         surname,
         'given-name' = asn1_NOVALUE,
         initials = asn1_NOVALUE,
         'generation-qualifier' = asn1_NOVALUE
        }).

-record('BuiltInDomainDefinedAttribute',
        {
         type,
         value
        }).

-record('ExtensionAttribute',
        {
         'extension-attribute-type',
         'extension-attribute-value'
        }).

-record('PDSParameter',
        {
         'printable-string' = asn1_NOVALUE,
         'teletex-string' = asn1_NOVALUE
        }).

-record('PresentationAddress',
        {
         pSelector = asn1_NOVALUE,
         sSelector = asn1_NOVALUE,
         tSelector = asn1_NOVALUE,
         nAddresses
        }).

-record('TeletexDomainDefinedAttribute',
        {
         type,
         value
        }).

-define('ubMax', 32768).
-define('ub-match', 128).
-define('ub-common-name-length', 64).
-define('ub-country-name-alpha-length', 2).
-define('ub-country-name-numeric-length', 3).
-define('ub-domain-defined-attributes', 4).
-define('ub-domain-defined-attribute-type-length', 8).
-define('ub-domain-defined-attribute-value-length', 128).
-define('ub-domain-name-length', 16).
-define('ub-extension-attributes', 256).
-define('ub-e163-4-number-length', 15).
-define('ub-e163-4-sub-address-length', 40).
-define('ub-generation-qualifier-length', 3).
-define('ub-given-name-length', 16).
-define('ub-initials-length', 5).
-define('ub-integer-options', 256).
-define('ub-numeric-user-id-length', 32).
-define('ub-organization-name-length', 64).
-define('ub-organizational-unit-name-length', 32).
-define('ub-organizational-units', 4).
-define('ub-pds-name-length', 16).
-define('ub-pds-parameter-length', 30).
-define('ub-pds-physical-address-lines', 6).
-define('ub-postal-code-length', 16).
-define('ub-surname-length', 40).
-define('ub-terminal-id-length', 24).
-define('ub-unformatted-address-length', 180).
-define('ub-x121-address-length', 16).

-define('ub-state-name', 128).
-define('ub-organization-name', 64).
-define('ub-organizational-unit-name', 64).
-define('ub-title', 64).
-define('ub-serial-number', 64).
-define('ub-pseudonym', 128).
-define('ub-emailaddress-length', 255).
-define('ub-locality-name', 128).
-define('ub-common-name', 64).
-define('ub-name', 32768).
-endif. % -ifdef(public_key).
