<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Public-Key Records

This chapter briefly describes Erlang records derived from ASN.1 specifications
used to handle public key infrastructure. The scope is to describe the data
types of each component, not the semantics. For information on the semantics,
refer to the relevant standards and RFCs linked in the sections below.

Use the following include directive to get access to the records and constant
macros described in the following sections:

```erlang
 -include_lib("public_key/include/public_key.hrl").
```

## Data Types

Common non-standard Erlang data types used to describe the record fields in the
following sections and which are not defined in the Public Key
[Reference Manual](`m:public_key`) follows here:

- **`time() =`** - `utc_time() | general_time()`

- **`utc_time() =`** - `{utcTime, "YYMMDDHHMMSSZ"}`

- **`general_time() =`** - `{generalTime, "YYYYMMDDHHMMSSZ"}`

- **`general_name() =`** - `{rfc822Name, string()}`

  `| {dNSName, string()}`

  `| {x400Address, string()}`

  `| {directoryName, {rdnSequence, [#AttributeTypeAndValue'{}]}}`

  `| {ediPartyName, special_string()}`

  `| {ediPartyName, special_string(), special_string()}`

  `| {uniformResourceIdentifier, string()}`

  `| {iPAddress, string()}`

  `| {registeredId, oid()}`

  `| {otherName, term()}`

- **`special_string() =`** - `{teletexString, string()}`

  `| {printableString, string()}`

  `| {universalString, string()}`

  `| {utf8String, binary()}`

  `| {bmpString, string()}`

- **`dist_reason() =`** - `unused`

  `| keyCompromise`

  `| cACompromise`

  `| affiliationChanged`

  `| superseded`

  `| cessationOfOperation`

  `| certificateHold`

  `| privilegeWithdrawn`

  `| aACompromise`

- **`OID_macro() =`** - `?OID_name()`

- **`OID_name() =`** - `t:atom/0`

## RSA

Erlang representation of
[Rivest-Shamir-Adleman cryptosystem (RSA)](http://www.ietf.org/rfc/rfc3447.txt)
keys follows:

```erlang
#'RSAPublicKey'{
	  modulus,       % integer()
	  publicExponent % integer()
	  }.

#'RSAPrivateKey'{
          version,         % two-prime | multi
	  modulus,         % integer()
	  publicExponent,  % integer()
	  privateExponent, % integer()
	  prime1,          % integer()
	  prime2,          % integer()
	  exponent1,       % integer()
	  exponent2,       % integer()
	  coefficient,     % integer()
	  otherPrimeInfos  % [#OtherPrimeInfo{}] | asn1_NOVALUE
	 }.

#'OtherPrimeInfo'{
	prime,           % integer()
	exponent,        % integer()
	coefficient      % integer()
 	}.

#'RSASSA-PSS-params'{hashAlgorithm,     % #'HashAlgorithm'{}},
	             maskGenAlgorithm,  % #'MaskGenAlgorithm'{}},
		     saltLength,        % integer(),
		     trailerField,      % integer()
		     }.

#'HashAlgorithm'{algorithm,  % oid()
                 parameters  % defaults to asn1_NOVALUE
                 }.

#'MaskGenAlgorithm'{algorithm,  % oid()
                    parameters, % defaults to asn1_NOVALUE
                   }.
```

## DSA

Erlang representation of
[Digital Signature Algorithm (DSA)](http://www.ietf.org/rfc/rfc6979.txt) keys

```erlang

#'DSAPrivateKey',{
	  version,      % integer()
	  p,            % integer()
	  q,            % integer()
	  g,            % integer()
	  y,            % integer()
	  x             % integer()
	  }.

#'Dss-Parms',{
         p,         % integer()
	 q,         % integer()
	 g          % integer()
	 }.
```

## ECDSA and EDDSA

Erlang representation of
[Elliptic Curve Digital Signature Algorithm (ECDSA)](http://www.ietf.org/rfc/rfc6979.txt)
and
[Edwards-Curve Digital Signature Algorithm (EDDSA)](https://tools.ietf.org/html/rfc8032)
where parameters in the private key will be
`{namedCurve, ?'id-Ed25519' | ?'id-Ed448'}`.

```erlang

#'ECPrivateKey'{
          version,       % integer()
	  privateKey,    % binary()
          parameters,    % {ecParameters, #'ECParameters'{}} |
                         % {namedCurve, Oid::tuple()} |
                         % {implicitlyCA, 'NULL'}
	  publicKey      % bitstring()
	  }.

#'ECParameters'{
      version,    % integer()
      fieldID,    % #'FieldID'{}
      curve,      % #'Curve'{}
      base,       % binary()
      order,      % integer()
      cofactor    % integer()
      }.

#'Curve'{
	a,        % binary()
	b,        % binary()
	seed      % bitstring() - optional

	}.

#'FieldID'{
	fieldType,    % oid()
	parameters    % Depending on fieldType
	}.

#'ECPoint'{
      point %  binary() - the public key
      }.
```

## PKIX Certificates

Erlang representation of PKIX certificates derived from ASN.1 specifications see
also [X509 certificates (RFC 5280)](http://www.ietf.org/rfc/rfc5280.txt), also
referred to as `plain` type, are as follows:

```erlang
#'Certificate'{
		tbsCertificate,        % #'TBSCertificate'{}
		signatureAlgorithm,    % #'AlgorithmIdentifier'{}
		signature              % bitstring()
	       }.

#'TBSCertificate'{
	  version,              % v1 | v2 | v3
	  serialNumber,         % integer()
	  signature,            % #'AlgorithmIdentifier'{}
	  issuer,               % {rdnSequence, [#AttributeTypeAndValue'{}]}
	  validity,             % #'Validity'{}
	  subject,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
	  subjectPublicKeyInfo, % #'SubjectPublicKeyInfo'{}
	  issuerUniqueID,       % binary() | asn1_novalue
	  subjectUniqueID,      % binary() | asn1_novalue
	  extensions            % [#'Extension'{}]
	 }.

#'AlgorithmIdentifier'{
	  algorithm,  % oid()
	  parameters  % der_encoded()
	 }.
```

Erlang alternate representation of PKIX certificate, also referred to as `otp`
type

```erlang
#'OTPCertificate'{
		tbsCertificate,        % #'OTPTBSCertificate'{}
		signatureAlgorithm,    % #'SignatureAlgorithm'
		signature              % bitstring()
	       }.

#'OTPTBSCertificate'{
	  version,              % v1 | v2 | v3
	  serialNumber,         % integer()
	  signature,            % #'SignatureAlgorithm'
	  issuer,               % {rdnSequence, [#AttributeTypeAndValue'{}]}
	  validity,             % #'Validity'{}
	  subject,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
	  subjectPublicKeyInfo, % #'OTPSubjectPublicKeyInfo'{}
	  issuerUniqueID,       % binary() | asn1_novalue
	  subjectUniqueID,      % binary() | asn1_novalue
	  extensions            % [#'Extension'{}]
	 }.

#'SignatureAlgorithm'{
	  algorithm,  % id_signature_algorithm()
	  parameters  % asn1_novalue | #'Dss-Parms'{}
	 }.
```

`id_signature_algorithm() = OID_macro()`

The available OID names are as follows:

| _OID Name_                                   |
| -------------------------------------------- |
| id-dsa-with-sha1                             |
| id-dsaWithSHA1 (ISO or OID to above)         |
| md2WithRSAEncryption                         |
| md5WithRSAEncryption                         |
| sha1WithRSAEncryption                        |
| sha-1WithRSAEncryption (ISO or OID to above) |
| sha224WithRSAEncryption                      |
| sha256WithRSAEncryption                      |
| sha512WithRSAEncryption                      |
| ecdsa-with-SHA1                              |

_Table: Signature Algorithm OIDs_

The data type `'AttributeTypeAndValue'`, is represented as the following erlang
record:

```erlang
#'AttributeTypeAndValue'{
	  type,   % id_attributes()
	  value   % term()
	 }.
```

The attribute OID name atoms and their corresponding value types are as follows:

| _OID Name_                | _Value Type_                  |
| ------------------------- | ----------------------------- |
| id-at-name                | special_string()              |
| id-at-surname             | special_string()              |
| id-at-givenName           | special_string()              |
| id-at-initials            | special_string()              |
| id-at-generationQualifier | special_string()              |
| id-at-commonName          | special_string()              |
| id-at-localityName        | special_string()              |
| id-at-stateOrProvinceName | special_string()              |
| id-at-organizationName    | special_string()              |
| id-at-title               | special_string()              |
| id-at-dnQualifier         | \{printableString, string()\} |
| id-at-countryName         | \{printableString, string()\} |
| id-at-serialNumber        | \{printableString, string()\} |
| id-at-pseudonym           | special_string()              |

_Table: Attribute OIDs_

The data types `'Validity'`, `'SubjectPublicKeyInfo'`, and
`'SubjectPublicKeyInfoAlgorithm'` are represented as the following Erlang
records:

```erlang
#'Validity'{
	  notBefore, % time()
	  notAfter   % time()
	 }.

#'SubjectPublicKeyInfo'{
	  algorithm,       % #AlgorithmIdentifier{}
	  subjectPublicKey % binary()
	 }.

#'SubjectPublicKeyInfoAlgorithm'{
	  algorithm,  % id_public_key_algorithm()
	  parameters  % public_key_params()
	 }.
```

The public-key algorithm OID name atoms are as follows:

| _OID Name_              |
| ----------------------- |
| rsaEncryption           |
| id-dsa                  |
| dhpublicnumber          |
| id-keyExchangeAlgorithm |
| id-ecPublicKey          |

_Table: Public-Key Algorithm OIDs_

```erlang
#'Extension'{
	  extnID,    % id_extensions() | oid()
	  critical,  % boolean()
	  extnValue  % der_encoded()
	 }.
```

`id_extensions()`
[Standard Certificate Extensions](public_key_records.md#StdCertExt),
[Private Internet Extensions](public_key_records.md#PrivIntExt),
[CRL Extensions](public_key_records.md#CRLCertExt) and
[CRL Entry Extensions](public_key_records.md#CRLEntryExt).

[](){: #StdCertExt }

## Standard Certificate Extensions

The standard certificate extensions OID name atoms and their corresponding value
types are as follows:

| _OID Name_                       | _Value Type_                   |
| -------------------------------- | ------------------------------ |
| id-ce-authorityKeyIdentifier     | \#'AuthorityKeyIdentifier'\{\} |
| id-ce-subjectKeyIdentifier       | oid()                          |
| id-ce-keyUsage                   | \[key_usage()]                 |
| id-ce-privateKeyUsagePeriod      | \#'PrivateKeyUsagePeriod'\{\}  |
| id-ce-certificatePolicies        | \#'PolicyInformation'\{\}      |
| id-ce-policyMappings             | \#'PolicyMappings_SEQOF'\{\}   |
| id-ce-subjectAltName             | general_name()                 |
| id-ce-issuerAltName              | general_name()                 |
| id-ce-subjectDirectoryAttributes | \[#'Attribute'\{\}]            |
| id-ce-basicConstraints           | \#'BasicConstraints'\{\}       |
| id-ce-nameConstraints            | \#'NameConstraints'\{\}        |
| id-ce-policyConstraints          | \#'PolicyConstraints'\{\}      |
| id-ce-extKeyUsage                | \[id_key_purpose()]            |
| id-ce-cRLDistributionPoints      | \[#'DistributionPoint'\{\}]    |
| id-ce-inhibitAnyPolicy           | integer()                      |
| id-ce-freshestCRL                | \[#'DistributionPoint'\{\}]    |

_Table: Standard Certificate Extensions_

Here:

- **`key_usage()`** - =

  `digitalSignature`

  `| nonRepudiation`

  `| keyEncipherment`

  `| dataEncipherment`

  `| keyAgreement`

  `| keyCertSign`

  `| cRLSign`

  `| encipherOnly`

  `| decipherOnly`

And for `id_key_purpose()`:

| _OID Name_            |
| --------------------- |
| id-kp-serverAuth      |
| id-kp-clientAuth      |
| id-kp-codeSigning     |
| id-kp-emailProtection |
| id-kp-timeStamping    |
| id-kp-OCSPSigning     |

_Table: Key Purpose OIDs_

```erlang
#'AuthorityKeyIdentifier'{
	  keyIdentifier,	    % oid()
	  authorityCertIssuer,      % general_name()
	  authorityCertSerialNumber % integer()
	 }.

#'PrivateKeyUsagePeriod'{
	  notBefore,   % general_time()
	  notAfter     % general_time()
	 }.

#'PolicyInformation'{
	  policyIdentifier,  % oid()
	  policyQualifiers   % [#PolicyQualifierInfo{}]
	 }.

#'PolicyQualifierInfo'{
	  policyQualifierId,   % oid()
	  qualifier            % string() | #'UserNotice'{}
	 }.

#'UserNotice'{
         noticeRef,   % #'NoticeReference'{}
	 explicitText % string()
	 }.

#'NoticeReference'{
         organization,    % string()
	 noticeNumbers    % [integer()]
	 }.

#'PolicyMappings_SEQOF'{
	  issuerDomainPolicy,  % oid()
	  subjectDomainPolicy  % oid()
	 }.

#'Attribute'{
          type,  % oid()
	  values % [der_encoded()]
	  }).

#'BasicConstraints'{
	  cA,		    % boolean()
	  pathLenConstraint % integer()
	 }).

#'NameConstraints'{
	  permittedSubtrees, % [#'GeneralSubtree'{}]
	  excludedSubtrees   % [#'GeneralSubtree'{}]
	 }).

#'GeneralSubtree'{
	  base,    % general_name()
	  minimum, % integer()
	  maximum  % integer()
	 }).

#'PolicyConstraints'{
	  requireExplicitPolicy, % integer()
	  inhibitPolicyMapping   % integer()
	 }).

#'DistributionPoint'{
	  distributionPoint, % {fullName, [general_name()]} | {nameRelativeToCRLIssuer,
	  [#AttributeTypeAndValue{}]}
	  reasons,           % [dist_reason()]
	  cRLIssuer          % [general_name()]
	 }).
```

[](){: #PrivIntExt }

## Private Internet Extensions

The private internet extensions OID name atoms and their corresponding value
types are as follows:

| _OID Name_                | _Value Type_                |
| ------------------------- | --------------------------- |
| id-pe-authorityInfoAccess | \[#'AccessDescription'\{\}] |
| id-pe-subjectInfoAccess   | \[#'AccessDescription'\{\}] |

_Table: Private Internet Extensions_

```erlang
#'AccessDescription'{
           accessMethod,    % oid()
	   accessLocation   % general_name()
	 }).
```

## CRL and CRL Extensions Profile

Erlang representation of CRL and CRL extensions profile derived from ASN.1
specifications and RFC 5280 are as follows:

```erlang
#'CertificateList'{
          tbsCertList,        % #'TBSCertList{}
          signatureAlgorithm, % #'AlgorithmIdentifier'{}
          signature           % bitstring()
	  }).

#'TBSCertList'{
      version,             % v2 (if defined)
      signature,           % #AlgorithmIdentifier{}
      issuer,              % {rdnSequence, [#AttributeTypeAndValue'{}]}
      thisUpdate,          % time()
      nextUpdate,          % time()
      revokedCertificates, % [#'TBSCertList_revokedCertificates_SEQOF'{}]
      crlExtensions        % [#'Extension'{}]
      }).

#'TBSCertList_revokedCertificates_SEQOF'{
         userCertificate,      % integer()
 	 revocationDate,       % timer()
	 crlEntryExtensions    % [#'Extension'{}]
	 }).
```

[](){: #CRLCertExt }

### CRL Extensions

The CRL extensions OID name atoms and their corresponding value types are as
follows:

| _OID Name_                     | _Value Type_                                    |
| ------------------------------ | ----------------------------------------------- |
| id-ce-authorityKeyIdentifier   | \#'AuthorityKeyIdentifier\{\}                   |
| id-ce-issuerAltName            | \{rdnSequence, \[#AttributeTypeAndValue'\{\}]\} |
| id-ce-cRLNumber                | integer()                                       |
| id-ce-deltaCRLIndicator        | integer()                                       |
| id-ce-issuingDistributionPoint | \#'IssuingDistributionPoint'\{\}                |
| id-ce-freshestCRL              | \[#'Distributionpoint'\{\}]                     |

_Table: CRL Extensions_

Here, the data type `'IssuingDistributionPoint'` is represented as the following
Erlang record:

```erlang
#'IssuingDistributionPoint'{
          distributionPoint,         % {fullName, [general_name()]} | {nameRelativeToCRLIssuer,
	  [#AttributeTypeAndValue'{}]}
	  onlyContainsUserCerts,     % boolean()
	  onlyContainsCACerts,       % boolean()
	  onlySomeReasons,           % [dist_reason()]
	  indirectCRL,               % boolean()
	  onlyContainsAttributeCerts % boolean()
	  }).
```

[](){: #CRLEntryExt }

### CRL Entry Extensions

The CRL entry extensions OID name atoms and their corresponding value types are
as follows:

| _OID Name_                | _Value Type_   |
| ------------------------- | -------------- |
| id-ce-cRLReason           | crl_reason()   |
| id-ce-holdInstructionCode | oid()          |
| id-ce-invalidityDate      | general_time() |
| id-ce-certificateIssuer   | general_name() |

_Table: CRL Entry Extensions_

Here:

- **`crl_reason()`** - =

  `unspecified`

  `| keyCompromise`

  `| cACompromise`

  `| affiliationChanged`

  `| superseded`

  `| cessationOfOperation`

  `| certificateHold`

  `| removeFromCRL`

  `| privilegeWithdrawn`

  `| aACompromise`

[](){: #PKCS10 }

### PKCS#10 Certification Request

Erlang representation of a PKCS#10 certification request derived from ASN.1
specifications and RFC 5280 are as follows:

```text
#'CertificationRequest'{
          certificationRequestInfo #'CertificationRequestInfo'{},
	  signatureAlgorithm	   #'CertificationRequest_signatureAlgorithm'{}}.
	  signature                bitstring()
	  }

#'CertificationRequestInfo'{
	  version       atom(),
	  subject       {rdnSequence, [#AttributeTypeAndValue'{}]} ,
	  subjectPKInfo #'CertificationRequestInfo_subjectPKInfo'{},
	  attributes    [#'AttributePKCS-10' {}]
	  }

#'CertificationRequestInfo_subjectPKInfo'{
          algorithm		#'CertificationRequestInfo_subjectPKInfo_algorithm'{}
	  subjectPublicKey 	  bitstring()
	  }

#'CertificationRequestInfo_subjectPKInfo_algorithm'{
     algorithm = oid(),
     parameters = der_encoded()
}

#'CertificationRequest_signatureAlgorithm'{
     algorithm = oid(),
     parameters = der_encoded()
     }

#'AttributePKCS-10'{
    type = oid(),
    values = [der_encoded()]
}
```
