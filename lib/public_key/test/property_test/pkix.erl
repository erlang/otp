%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
%%

-module(pkix).

-compile(export_all).

-include_lib("common_test/include/ct_property_test.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("public_key/include/PKIXCRMF.hrl").
-include_lib("public_key/include/PKIXCMP.hrl").

-define(EMPTY_PARAM, {asn1_OPENTYPE, <<5,0>>}).

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

implicit_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, implicit_type(), {Type, implicit_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
explicit_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, explicit_type(), {Type, explicit_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
ocsp_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, ocsp_type(), {Type, ocsp_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
algorithm_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, algorithm_type(), {Type, algorithm_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
crmf_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, crmf_type(), {Type, crmf_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
cmp_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, cmp_type(), {Type, cmp_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
cms_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, cms_type(), {Type, cms_value(Type)}),
            encode_decode_check(PkixType, Decoded)).

encode_decode_check(PkixType, Decoded) ->
    try
        Encoded = public_key:der_encode(PkixType, Decoded),
        NewDecoded = public_key:der_decode(PkixType, Encoded),
        case Decoded == NewDecoded of
            true ->
                true;
            false ->
                io:format("Exp: ~p~nGot: ~p~n", [Decoded, NewDecoded]),
                false
        end
    catch
        _:_R:_ST  ->
            io:format("Exp: ~p~n Crash: ~p~nin: ~p ~n", [Decoded, _R, _ST]),
            false
    end.

%%--------------------------------------------------------------------
%% Generators --------------------------------------------------------
%%--------------------------------------------------------------------
implicit_type() ->
    elements(['AuthorityInfoAccessSyntax',
              'AuthorityKeyIdentifier',
              'BaseCRLNumber',
              'BasicConstraints',
              'CRLDistributionPoints',
              'CRLNumber',
              'CRLReason',
              'CertificateIssuer',
              'CertificatePolicies',
              'ExtKeyUsageSyntax',
              'FreshestCRL',
              'GeneralNames',
              'HoldInstructionCode',
              'InhibitAnyPolicy',
              'InvalidityDate',
              'IssuingDistributionPoint',
              'IssuerAltName',
              'KeyIdentifier',
              'KeyUsage',
              'NameConstraints',
              'PolicyConstraints',
              'PolicyMappings',
              'PrivateKeyUsagePeriod',
              'SubjectAltName',
              'SubjectInfoAccessSyntax',
              'SubjectKeyIdentifier']).

implicit_value('BasicConstraints') ->
    #'BasicConstraints'{cA = ?LET(CA, bool(), CA),
                        pathLenConstraint = ?LET(Len, choose(1, 10), Len)
                       };
implicit_value('ExtKeyUsageSyntax') ->
    ?LET(Usages, list(ext_key_usages()), lists:usort(Usages));
implicit_value('KeyUsage') ->
    ?LET(Usages, list(choose(0,8)),
         [key_usages_enum(Usage) || Usage <- lists:usort(Usages)]);
implicit_value('KeyIdentifier') ->
    ?LET(Bin, binary(), Bin);
implicit_value('AuthorityKeyIdentifier') ->
    #'AuthorityKeyIdentifier'{
       keyIdentifier = ?LET(Bin, binary(20), Bin),
       authorityCertIssuer = asn1_NOVALUE,
       authorityCertSerialNumber = asn1_NOVALUE
      };
implicit_value('PolicyConstraints') ->
    ?LET({Req, Inh}, {choose(0, 10), choose(0, 10)},
         #'PolicyConstraints'{
            requireExplicitPolicy = Req,
            inhibitPolicyMapping = Inh
           });
implicit_value('NameConstraints') ->
    #'NameConstraints'{
       permittedSubtrees = [general_subtree()],
       excludedSubtrees = asn1_NOVALUE
      };
implicit_value('CRLReason') ->
    elements([unspecified, keyCompromise, cACompromise,
              affiliationChanged, superseded, cessationOfOperation,
              certificateHold, removeFromCRL, privilegeWithdrawn,
              aACompromise]);
implicit_value('CRLNumber') ->
    choose(0, 65535);
implicit_value('GeneralNames') ->
    ?LET(Name, general_name(), [Name]);
implicit_value('CRLDistributionPoints') ->
    ?LET(Name, general_name(),
         [#'DistributionPoint'{
             distributionPoint = {fullName, [Name]},
             reasons = asn1_NOVALUE,
             cRLIssuer = asn1_NOVALUE
            }]);
implicit_value('IssuingDistributionPoint') ->
    ?LET(Name, general_name(),
         #'IssuingDistributionPoint'{
            distributionPoint = {fullName, [Name]},
            onlyContainsUserCerts = ?LET(B, bool(), B),
            onlyContainsCACerts = false,
            onlySomeReasons = asn1_NOVALUE,
            indirectCRL = false,
            onlyContainsAttributeCerts = false
           });
implicit_value('AuthorityInfoAccessSyntax') ->
    ?LET(Name, general_name(),
         [#'AccessDescription'{
             accessMethod = ?'id-ad-caIssuers',
             accessLocation = Name
            }]);
implicit_value('BaseCRLNumber') ->
    choose(0, 65535);
implicit_value('CertificateIssuer') ->
    ?LET(Name, general_names(), Name);
implicit_value('CertificatePolicies') ->
    [#'PolicyInformation'{
        policyIdentifier = ?'anyPolicy',
        policyQualifiers = asn1_NOVALUE
       }];
implicit_value('FreshestCRL') ->
    ?LET(Name, general_names(),
         [#'DistributionPoint'{
             distributionPoint = {fullName, Name},
             reasons = asn1_NOVALUE,
             cRLIssuer = opt_general_names()
            }]);
implicit_value('HoldInstructionCode') ->
    elements([?'id-holdinstruction-none',
              ?'id-holdinstruction-callissuer',
              ?'id-holdinstruction-reject']);
implicit_value('InhibitAnyPolicy') ->
    choose(0, 10);
implicit_value('InvalidityDate') ->
    "20200101000000Z";
implicit_value('IssuerAltName') ->
    ?LET(Names, general_names(), Names);
implicit_value('PolicyMappings') ->
    [#'PolicyMappings_SEQOF'{
        issuerDomainPolicy = ?'anyPolicy',
        subjectDomainPolicy = ?'anyPolicy'
       }];
implicit_value('PrivateKeyUsagePeriod') ->
    #'PrivateKeyUsagePeriod'{
       notBefore = "20100101000000Z",
       notAfter = "20301231235959Z"
      };
implicit_value('SubjectAltName') ->
    ?LET(Names, general_names(), Names);
implicit_value('SubjectInfoAccessSyntax') ->
    ?LET(Name, general_name(),
         [#'AccessDescription'{
             accessMethod = ?'id-ad-caRepository',
             accessLocation = Name
            }]);
implicit_value('SubjectKeyIdentifier') ->
    ?LET(Bin, binary(20), Bin).

general_subtree() ->
    #'GeneralSubtree'{
       base = general_name(),
       minimum = 0,
       maximum = asn1_NOVALUE
      }.

opt_general_names() ->
    oneof([asn1_NOVALUE, general_names()]).

general_names() ->
    ?LET(N, choose(1,3), [general_name() || _ <- lists:seq(1, N)]).

general_name() ->
    oneof([{rfc822Name, "test@example.com"},
           {dNSName, "example.com"},
           {uniformResourceIdentifier, "http://example.com"},
           directoryName()]).

ext_key_usages() ->
    elements([?'id-kp-serverAuth',
              ?'id-kp-clientAuth',
              ?'id-kp-codeSigning',
              ?'id-kp-emailProtection',
              ?'id-kp-timeStamping',
              ?'id-kp-OCSPSigning'
             ]).

key_usages_enum(0) ->
    digitalSignature;
key_usages_enum(1) ->
    nonRepudiation;
key_usages_enum(2) ->
    keyEncipherment;
key_usages_enum(3) ->
    dataEncipherment;
key_usages_enum(4) ->
    keyAgreement;
key_usages_enum(5) ->
    keyCertSign;
key_usages_enum(6) ->
    cRLSign;
key_usages_enum(7) ->
    encipherOnly;
key_usages_enum(8) ->
    decipherOnly.

explicit_type() ->
    elements(['Certificate',
              'CertificateList',
              'Name',
              'SubjectPublicKeyInfo',
              'TBSCertificate',
              'TBSCertList',
              'Validity',
              'X520CommonName'
             ]).

explicit_value('Certificate') ->
    ?LET(TBS, explicit_value('TBSCertificate'),
         #'Certificate'{
            tbsCertificate = TBS,
            signatureAlgorithm =
                #'Certificate_algorithmIdentifier'{
                   algorithm = TBS#'TBSCertificate'.signature#'TBSCertificate_signature'.algorithm,
                   parameters = TBS#'TBSCertificate'.signature#'TBSCertificate_signature'.parameters},
            signature = <<0, 1:256>>});
explicit_value('TBSCertificate') ->
    ?LET({AlgId, Issuer, Subject, SPKI},
         {algorithm_identifier(), rdn_sequence(), rdn_sequence(),
          explicit_value('SubjectPublicKeyInfo')},
         #'TBSCertificate'{
            version = v3,
            serialNumber = 1,
            signature = #'TBSCertificate_signature'{
                           algorithm = AlgId#'AlgorithmIdentifier'.algorithm,
                           parameters = AlgId#'AlgorithmIdentifier'.parameters},
            issuer = Issuer,
            validity = #'Validity'{notBefore = {utcTime, "200101000000Z"},
                                   notAfter = {utcTime, "300101000000Z"}},
            subject = Subject,
            subjectPublicKeyInfo = SPKI,
            extensions = asn1_NOVALUE});
explicit_value('CertificateList') ->
    ?LET(TBSCertList, explicit_value('TBSCertList'),
         #'CertificateList'{
            tbsCertList = TBSCertList,
            signatureAlgorithm =
                #'CertificateList_algorithmIdentifier'{
                   algorithm = TBSCertList#'TBSCertList'.signature#'TBSCertList_signature'.algorithm,
                   parameters = TBSCertList#'TBSCertList'.signature#'TBSCertList_signature'.parameters},
            signature = <<0, 1:256>>});
explicit_value('Validity') ->
    #'Validity'{ notBefore = {utcTime,"080109082930Z"}, notAfter = {utcTime,"171117082930Z"}};
explicit_value('Name') ->
    rdn_sequence();
explicit_value('X520CommonName') ->
    ?LET(Len, choose(1, ?'ub-common-name'),
         oneof([{printableString, printable_string(Len)},
                {utf8String, utf8(Len)}]));
explicit_value('SubjectPublicKeyInfo') ->
    ?LET(AlgId, algorithm_identifier(),
         #'SubjectPublicKeyInfo'{
            algorithm = AlgId,
            subjectPublicKey = <<0, 1:256>>});
explicit_value('TBSCertList') ->
    ?LET({AlgId, Issuer}, {algorithm_identifier(), rdn_sequence()},
         #'TBSCertList'{
            version = v2,
            signature = #'TBSCertList_signature'{
                           algorithm = AlgId#'AlgorithmIdentifier'.algorithm,
                           parameters = AlgId#'AlgorithmIdentifier'.parameters},
            issuer = Issuer,
            thisUpdate = {utcTime, "200101000000Z"},
            nextUpdate = asn1_NOVALUE,
            revokedCertificates = asn1_NOVALUE,
            crlExtensions = asn1_NOVALUE}).

rdn_sequence() ->
    ?LET(Ids, ?SUCHTHAT(X, list(id_attrs()), X =/= []),
         {rdnSequence, [[atter_value(Id)] || Id <- Ids]}).

algorithm_identifier() ->
    Algos = [?'rsaEncryption', ?'id-RSASSA-PSS', ?'id-dsa', ?'id-ecPublicKey',
             ?'id-Ed25519', ?'id-Ed448', ?'id-ml-dsa-44',?'id-ml-dsa-65',?'id-ml-dsa-87'],
    ?LET(Algo, oneof(Algos),
         #'AlgorithmIdentifier'{algorithm = Algo, parameters = asn1_NOVALUE}).
algorithm_type() ->
    elements(['DHParameter',
              'DSA-Params',
              'DSAPublicKey',
              'ECDSA-Sig-Value',
              'PrivateKeyInfo',
              'RSAPublicKey']).

algorithm_value('RSAPublicKey') ->
    #'RSAPublicKey'{modulus = 1000003,
                    publicExponent = 65537};
algorithm_value('DSA-Params') ->
    {params, #'Dss-Parms'{p = 1000003,
                          q = 1009,
                          g = 2}};
algorithm_value('DSAPublicKey') ->
    42;
algorithm_value('DHParameter') ->
    #'DHParameter'{prime = 1000003,
                   base = 2,
                   privateValueLength = asn1_NOVALUE};
algorithm_value('ECDSA-Sig-Value') ->
    #'ECDSA-Sig-Value'{r = 123456,
                       s = 654321};
algorithm_value('PrivateKeyInfo') ->
    oneof([private_key_rsa(),
           private_key_rsa_pss(),
           private_key_dsa(),
           private_key_ec(),
           private_key_ed25519(),
           private_key_ed448(),
           private_key_mldsa(),
           private_key_slhdsa()]).

private_key_rsa() ->
    #'RSAPrivateKey'{version = 'two-prime',
                     modulus = 1000003,
                     publicExponent = 65537,
                     privateExponent = 123456,
                     prime1 = 1009,
                     prime2 = 1013,
                     exponent1 = 101,
                     exponent2 = 103,
                     coefficient = 42}.

private_key_dsa() ->
    #'DSAPrivateKey'{version = 1,
                     p = 1000003,
                     q = 1009,
                     g = 2,
                     x = 42}.

private_key_ec() ->
    #'ECPrivateKey'{version = ecPrivkeyVer1,
                    privateKey = <<1:256>>,
                    parameters = {namedCurve, ?'secp256r1'},
                    publicKey = asn1_NOVALUE,
                    attributes = asn1_NOVALUE}.

private_key_ed25519() ->
    #'ECPrivateKey'{version = 1,
                    privateKey = <<1:256>>,
                    parameters = {namedCurve, ?'id-Ed25519'},
                    publicKey = asn1_NOVALUE,
                    attributes = asn1_NOVALUE}.

private_key_ed448() ->
    #'ECPrivateKey'{version = 1,
                    privateKey = <<1:456>>,
                    parameters = {namedCurve, ?'id-Ed448'},
                    publicKey = asn1_NOVALUE,
                    attributes = asn1_NOVALUE}.

private_key_rsa_pss() ->
    {#'RSAPrivateKey'{version = 'two-prime',
                      modulus = 1000003,
                      publicExponent = 65537,
                      privateExponent = 123456,
                      prime1 = 1009,
                      prime2 = 1013,
                      exponent1 = 101,
                      exponent2 = 103,
                      coefficient = 42},
     #'RSASSA-PSS-params'{
        %% With Default Values, i.e. asn1_DEFAULT, they come back as values
        %% after decode/encode
        hashAlgorithm = {'HashAlgorithm',{1,3,14,3,2,26},'NULL'},
        maskGenAlgorithm = {'MaskGenAlgorithm',{1,2,840,113549,1,1,8},
                            {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}},
        saltLength = 20,
        trailerField = 1}}.

private_key_mldsa() ->
    ?LET(Alg, elements([mldsa44, mldsa65, mldsa87]),
         #'ML-DSAPrivateKey'{algorithm = Alg,
                             seed = <<1:256>>,
                             expandedkey = <<>>}).

private_key_slhdsa() ->
    ?LET(Alg, elements([slh_dsa_sha2_128s, slh_dsa_sha2_128f,
                        slh_dsa_sha2_192s, slh_dsa_sha2_192f,
                        slh_dsa_sha2_256s, slh_dsa_sha2_256f,
                        slh_dsa_shake_128s, slh_dsa_shake_128f,
                        slh_dsa_shake_192s, slh_dsa_shake_192f,
                        slh_dsa_shake_256s, slh_dsa_shake_256f]),
         #'SLH-DSAPrivateKey'{algorithm = Alg,
                              key = <<1:512>>}).

crmf_type() ->
    elements(['CertificationRequest',
              'CertificationRequestInfo',
              'CertRequest',
              'OldCertId']).

crmf_value('CertificationRequest') ->
    ?LET({CRI, AlgId}, {crmf_value('CertificationRequestInfo'), algorithm_identifier()},
         #'CertificationRequest'{
            certificationRequestInfo = CRI,
            signatureAlgorithm =
                #'CertificationRequest_signatureAlgorithm'{
                   algorithm = AlgId#'AlgorithmIdentifier'.algorithm,
                   parameters = AlgId#'AlgorithmIdentifier'.parameters},
            signature = <<0, 1:256>>});
crmf_value('CertificationRequestInfo') ->
    ?LET({AlgId, Subject}, {algorithm_identifier(), rdn_sequence()},
         #'CertificationRequestInfo'{
            version = v1,
            subject = Subject,
            subjectPKInfo =
                #'CertificationRequestInfo_subjectPKInfo'{
                   algorithm =
                       #'CertificationRequestInfo_subjectPKInfo_algorithm'{
                          algorithm = AlgId#'AlgorithmIdentifier'.algorithm,
                          parameters = AlgId#'AlgorithmIdentifier'.parameters},
                   subjectPublicKey = <<0, 1:256>>},
            attributes = []});
crmf_value('CertRequest') ->
    ?LET(CS,  list(attribute_type_and_value()),
         #'CertRequest'{
            certReqId = 0,
            certTemplate = #'CertTemplate'{},
            controls = CS});
crmf_value('OldCertId') ->
    ?LET(Name, rdn_sequence(),
         #'CertId'{
            issuer = {directoryName, Name},
            serialNumber = 1}).

attribute_type_and_value() ->
    SomeVariants =
        [{?'id-regCtrl-regToken', <<"token">>},
         {?'id-regCtrl-oldCertID',
          ?LET(Name, rdn_sequence(),
               #'CertId'{issuer = {directoryName, Name}, serialNumber = 1})},
         {?'id-regCtrl-protocolEncrKey', explicit_value('SubjectPublicKeyInfo')}
        ],
    ?LET({Type,Value}, oneof(SomeVariants),
         #'AttributeTypeAndValue'{type = Type, value = Value}).


cmp_type() ->
    elements(['PBMParameter',
              'PKIMessage',
              'ProtectedPart']).

cmp_value('PBMParameter') ->
    #'PBMParameter'{
       salt = <<1:128>>,
       owf = #'PBMParameter_owf'{algorithm = ?'id-sha256',
                                  parameters = ?EMPTY_PARAM},
       iterationCount = 1000,
       mac = #'PBMParameter_mac'{algorithm = ?'id-hmacWithSHA256',
                                  parameters = ?EMPTY_PARAM}};
cmp_value('PKIMessage') ->
    ?LET({Header, Body}, {cmp_header(), cmp_body()},
         #'PKIMessage'{
            header = Header,
            body = Body,
            protection = asn1_NOVALUE,
            extraCerts = asn1_NOVALUE});
cmp_value('ProtectedPart') ->
    ?LET({Header, Body}, {cmp_header(), cmp_body()},
         #'ProtectedPart'{
            header = Header,
            body = Body}).

cmp_body() ->
    oneof([{pkiconf, 'NULL'},
           {ir, cmp_cert_req_messages()},
           {cr, cmp_cert_req_messages()},
           {p10cr, crmf_value('CertificationRequest')},
           {error, #'ErrorMsgContent'{
                       pKIStatusInfo = #'PKIStatusInfo'{status = rejection},
                       errorCode = asn1_NOVALUE,
                       errorDetails = asn1_NOVALUE}},
           {pollReq, [#'PollReqContent_SEQOF'{certReqId = 0}]},
           {pollRep, [#'PollRepContent_SEQOF'{certReqId = 0,
                                               checkAfter = 60,
                                               reason = asn1_NOVALUE}]}
          ]).

cmp_cert_req_messages() ->
    [#'CertReqMsg'{
        certReq = crmf_value('CertRequest'),
        popo = asn1_NOVALUE,
        regInfo = asn1_NOVALUE}].

cmp_header() ->
    ?LET({Sender, Recipient}, {rdn_sequence(), rdn_sequence()},
         #'PKIHeader'{
            pvno = cmp2021,
            sender = {directoryName, Sender},
            recipient = {directoryName, Recipient}}).

cms_type() ->
    elements(['AuthenticatedData',
              'ContentInfo',
              'DigestedData',
              'EncapsulatedContentInfo',
              'EncryptedData',
              'EnvelopedData',
              'IssuerAndSerialNumber',
              'KEKRecipientInfo',
              'KeyTransRecipientInfo',
              'PasswordRecipientInfo',
              'RecipientInfo',
              'SignedData',
              'SignerInfo']).

cms_value('ContentInfo') ->
    #'ContentInfo'{
       contentType = ?'id-data',
       content = <<1,2,3>>};
cms_value('EncapsulatedContentInfo') ->
    #'EncapsulatedContentInfo'{
       eContentType = ?'id-data',
       eContent = <<1,2,3>>};
cms_value('IssuerAndSerialNumber') ->
    ?LET(Name, rdn_sequence(),
         #'IssuerAndSerialNumber'{
            issuer = Name,
            serialNumber = 1});
cms_value('KeyTransRecipientInfo') ->
    ?LET(Name, rdn_sequence(),
         #'KeyTransRecipientInfo'{
            version = v0,
            rid = {issuerAndSerialNumber,
                   #'IssuerAndSerialNumber'{issuer = Name,
                                            serialNumber = 1}},
            keyEncryptionAlgorithm =
                #'KeyTransRecipientInfo_keyEncryptionAlgorithm'{
                   algorithm = ?'rsaEncryption',
                   parameters = 'NULL'},
            encryptedKey = <<1:256>>});
cms_value('KEKRecipientInfo') ->
    #'KEKRecipientInfo'{
       version = v4,
       kekid = #'KEKIdentifier'{keyIdentifier = <<1:128>>,
                                date = asn1_NOVALUE,
                                other = asn1_NOVALUE},
       keyEncryptionAlgorithm =
           #'KeyEncryptionAlgorithmIdentifier'{
              algorithm = ?'id-aes128-wrap',
              parameters = asn1_NOVALUE},
       encryptedKey = <<1:256>>};
cms_value('PasswordRecipientInfo') ->
    #'PasswordRecipientInfo'{
       version = v0,
       keyDerivationAlgorithm = asn1_NOVALUE,
       keyEncryptionAlgorithm =
           #'KeyEncryptionAlgorithmIdentifier'{
              algorithm = ?'id-aes128-wrap',
              parameters = asn1_NOVALUE},
       encryptedKey = <<1:256>>};
cms_value('RecipientInfo') ->
    oneof([?LET(V, cms_value('KeyTransRecipientInfo'), {ktri, V}),
           ?LET(V, cms_value('KEKRecipientInfo'), {kekri, V}),
           ?LET(V, cms_value('PasswordRecipientInfo'), {pwri, V})]);
cms_value('DigestedData') ->
    #'DigestedData'{
       version = v0,
       digestAlgorithm =
           #'DigestAlgorithmIdentifier'{algorithm = ?'id-sha256',
                                        parameters = ?EMPTY_PARAM},
       encapContentInfo =
           #'EncapsulatedContentInfo'{
              eContentType = ?'id-data',
              eContent = <<1,2,3>>},
       digest = <<1:256>>};
cms_value('EncryptedData') ->
    #'EncryptedData'{
       version = v0,
       encryptedContentInfo =
           #'EncryptedContentInfo'{
              contentType = ?'id-data',
              contentEncryptionAlgorithm =
                  #'ContentEncryptionAlgorithmIdentifier'{
                     algorithm = ?'id-aes128-CBC',
                     parameters = {asn1_OPENTYPE, <<4,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>}},
              encryptedContent = <<1,2,3,4>>},
       unprotectedAttrs = asn1_NOVALUE};
cms_value('SignerInfo') ->
    ?LET(Name, rdn_sequence(),
         #'SignerInfo'{
            version = v1,
            sid = {issuerAndSerialNumber,
                   #'IssuerAndSerialNumber'{issuer = Name,
                                            serialNumber = 1}},
            digestAlgorithm =
                #'DigestAlgorithmIdentifier'{algorithm = ?'id-sha256',
                                             parameters = ?EMPTY_PARAM},
            signedAttrs = asn1_NOVALUE,
            signatureAlgorithm =
                #'SignatureAlgorithmIdentifier'{algorithm = ?'sha256WithRSAEncryption',
                                               parameters = ?EMPTY_PARAM},
            signature = <<1:256>>,
            unsignedAttrs = asn1_NOVALUE});
cms_value('SignedData') ->
    ?LET(SignerInfo, cms_value('SignerInfo'),
         #'SignedData'{
            version = v1,
            digestAlgorithms =
                [#'DigestAlgorithmIdentifier'{algorithm = ?'id-sha256',
                                              parameters = ?EMPTY_PARAM}],
            encapContentInfo =
                #'EncapsulatedContentInfo'{
                   eContentType = ?'id-data',
                   eContent = <<1,2,3>>},
            certificates = asn1_NOVALUE,
            crls = asn1_NOVALUE,
            signerInfos = [SignerInfo]});
cms_value('EnvelopedData') ->
    ?LET(RI, cms_value('RecipientInfo'),
         #'EnvelopedData'{
            version = v0,
            recipientInfos = [RI],
            encryptedContentInfo =
                #'EncryptedContentInfo'{
                   contentType = ?'id-data',
                   contentEncryptionAlgorithm =
                       #'ContentEncryptionAlgorithmIdentifier'{
                          algorithm = ?'id-aes128-CBC',
                          parameters = {asn1_OPENTYPE, <<4,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>}},
                   encryptedContent = <<1,2,3,4>>}});
cms_value('AuthenticatedData') ->
    ?LET(RI, cms_value('RecipientInfo'),
         #'AuthenticatedData'{
            version = v0,
            recipientInfos = [RI],
            macAlgorithm =
                #'MessageAuthenticationCodeAlgorithm'{
                   algorithm = ?'id-hmacWithSHA256',
                   parameters = ?EMPTY_PARAM},
            encapContentInfo =
                #'EncapsulatedContentInfo'{
                   eContentType = ?'id-data',
                   eContent = <<1,2,3>>},
            mac = <<1:256>>}).

ocsp_type() ->
    elements(['BasicOCSPResponse',
              'ResponseData',
              'CertID',
              'Nonce',
              'OCSPRequest',
              'OCSPResponse'
             ]).

ocsp_value('BasicOCSPResponse') ->
    ?LET(RespData, ocsp_value('ResponseData'),
         #'BasicOCSPResponse'{
            tbsResponseData = RespData,
            signatureAlgorithm =
                #'BasicOCSPResponse_signatureAlgorithm'{
                   algorithm = ?'sha256WithRSAEncryption',
                   parameters = ?EMPTY_PARAM},
            signature = <<0, 1:256>>,
            certs = asn1_NOVALUE});
ocsp_value('ResponseData') ->
    ?LET({HashAlgo, Params}, hash_algo(),
         #'ResponseData'{
            version = 0,
            responderID = {byKey, <<1:160>>},
            producedAt = "20200101000000Z",
            responses =
                [#'SingleResponse'{
                    certID =
                        #'CertID'{
                           hashAlgorithm =
                               #'CertID_hashAlgorithm'{algorithm = HashAlgo,
                                                       parameters = Params},
                           issuerNameHash = <<1:160>>,
                           issuerKeyHash = <<1:160>>,
                           serialNumber = 1},
                    certStatus = {good, 'NULL'},
                    thisUpdate = "20200101000000Z",
                    nextUpdate = asn1_NOVALUE,
                    singleExtensions = asn1_NOVALUE}],
            responseExtensions = asn1_NOVALUE});
ocsp_value('CertID') ->
    ?LET({HashAlgo, Params}, hash_algo(),
         #'CertID'{hashAlgorithm =
                       #'CertID_hashAlgorithm'{algorithm = HashAlgo,
                                               parameters = Params},
                   issuerNameHash = hash(HashAlgo),
                   issuerKeyHash = hash(HashAlgo),
                   serialNumber = choose(1, 65535)});
ocsp_value('Nonce') ->
    ?LET(Len, choose(1, 128), binary(Len));
ocsp_value('OCSPRequest') ->
    TBSRequest = #'TBSRequest'{
                    version = 0,
                    requestorName = directoryName(),
                    requestList = requestor_list(),
                    requestExtensions = extensions()
                   },
    #'OCSPRequest'{
       tbsRequest = TBSRequest,
       optionalSignature = asn1_NOVALUE
      };
ocsp_value('OCSPResponse') ->
    ?LET(Status, elements([successful, malformedRequest, internalError,
                           tryLater, sigRequired, unauthorized]),
         #'OCSPResponse'{
            responseStatus = Status,
            responseBytes = asn1_NOVALUE
           }).

directoryName() ->
    ?LET(Name, rdn_sequence(), {directoryName, Name}).

requestor_list() ->
    ?LET(HashAlgo, hash_algorithm(), requestor_list(HashAlgo)).

requestor_list({HashAlgo, Params}) ->
    [#'Request'{
        reqCert =
            #'CertID'{hashAlgorithm =
                          #'CertID_hashAlgorithm'{algorithm = HashAlgo,
                                                  parameters = Params},
                      issuerNameHash = hash(HashAlgo),
                      issuerKeyHash = hash(HashAlgo),
                      serialNumber = choose(1, 6553)},
        singleRequestExtensions = asn1_NOVALUE}].

hash_algorithm() ->
    ?LET(HashAlgo, hash_algo(), HashAlgo).

hash_algo() ->
    elements([{?'id-sha1', ?EMPTY_PARAM},
              {?'id-sha256', ?EMPTY_PARAM},
              {?'id-sha384', ?EMPTY_PARAM},
              {?'id-sha512', ?EMPTY_PARAM}]).

hash(?'id-sha1') ->
    ?LET(Value, binary(20), Value);
hash(?'id-sha256') ->
    ?LET(Value, binary(32), Value);
hash(?'id-sha384') ->
    ?LET(Value, binary(48), Value);
hash(?'id-sha512') ->
    ?LET(Value, binary(64), Value).

extensions() ->
    [#'Extension'{
        extnID = ?'id-pkix-ocsp-nonce',
        critical = false,
        extnValue = ocsp_nonce()}].

ocsp_nonce() ->
    ?LET(Len, choose(1, 128), binary(Len)).

id_attrs()->
    elements([?'id-at-surname',
              ?'id-at-givenName',
              ?'id-at-initials',
              ?'id-at-generationQualifier',
              ?'id-at-commonName',
              ?'id-at-localityName',
              ?'id-at-stateOrProvinceName',
              ?'id-at-organizationName',
              ?'id-at-title',
              ?'id-at-dnQualifier',
              ?'id-at-countryName',
              ?'id-at-serialNumber',
              ?'id-at-pseudonym',
              ?'id-domainComponent',
              ?'id-emailAddress',
              ?'id-at-organizationalUnitName'
             ]).

atter_value(?'id-at-countryName' = Name) ->
    ?LET({Capital1, Capital2}, {upper_case(), upper_case()},
         #'AttributeTypeAndValue'{
            type = Name,
            value = [Capital1, Capital2]
           });
atter_value(?'id-emailAddress' = Email) ->
    End = "@example.com",
    GenLen = ?'ub-emailaddress-length' - length(End),
    ?LET(Len, choose(1, GenLen),
         #'AttributeTypeAndValue'{
            type = Email,
            value = printable_string(Len, End)
           });
atter_value(Name) when Name == ?'id-at-surname';
                       Name == ?'id-at-givenName';
                       Name == ?'id-at-initials';
                       Name == ?'id-at-organizationName';
                       Name == ?'id-at-organizationalUnitName';
                       Name == ?'id-at-title' ->
    ?LET(Size, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = {utf8String, utf8(Size)}
           });
atter_value(Name) when Name == ?'id-at-localityName';
                       Name == ?'id-at-stateOrProvinceName';
                       Name == ?'id-at-pseudonym';
                       Name == ?'id-at-generationQualifier';
                       Name == ?'id-at-commonName' ->
    ?LET(Len, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = {printableString, printable_string(Len)}
           });

atter_value(Name) when Name == ?'id-domainComponent' ->
    ?LET(Len, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = ia5_string(Len)
           });
atter_value(Name) when Name == ?'id-at-serialNumber';
                       Name == ?'id-at-dnQualifier'->
    ?LET(Num, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = integer_to_list(Num)
           }).

upper_bound(?'id-at-surname') ->
    ?'ub-surname-length';
upper_bound(?'id-at-givenName') ->
    ?'ub-given-name-length';
upper_bound(?'id-at-initials') ->
    ?'ub-initials-length';
upper_bound(?'id-at-organizationalUnitName') ->
    ?'ub-organizational-unit-name';
upper_bound(?'id-at-organizationName') ->
    ?'ub-organization-name-length';
upper_bound(?'id-at-title') ->
    ?'ub-title';
upper_bound(?'id-at-localityName') ->
    ?'ub-locality-name';
upper_bound(?'id-at-stateOrProvinceName') ->
    ?'ub-state-name';
upper_bound(?'id-at-pseudonym') ->
    ?'ub-pseudonym';
upper_bound(?'id-at-generationQualifier') ->
    ?'ub-generation-qualifier-length';
upper_bound(?'id-at-commonName') ->
    ?'ub-common-name';
upper_bound(?'id-at-dnQualifier') ->
    ?'ub-domain-defined-attribute-value-length';
upper_bound(?'id-at-serialNumber') ->
    ?'ub-serial-number';
upper_bound(?'id-domainComponent') ->
    ?'ub-domain-name-length'.

upper_case() ->
    ?LET(ASCII, choose($A, $Z), ASCII).

lower_case() ->
    ?LET(ASCII, choose($a, $z), ASCII).

digits() ->
    ?LET(ASCII, choose($0, $9), ASCII).

misc_values() ->
    ?LET(ASCII, elements([$\r, $.,$,]), ASCII).

printable_string(Len)->
    ?LET(Value,
         ?SUCHTHAT(X, list(oneof([lower_case(), upper_case(), digits(), misc_values()])), X =/= []),
         Value).

printable_string(Len, Suffix)->
    ?LET(Value, ?SUCHTHAT(X, list(lower_case()), X =/= []), Value ++ Suffix).

ia5_string(Len)->
    ?LET(Value, ?SUCHTHAT(X, list(oneof([upper_case(), lower_case()])), X =/= []),
         Value).
