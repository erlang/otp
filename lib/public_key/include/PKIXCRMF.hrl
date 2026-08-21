%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-ifndef(_PKIXCRMF_HRL_).
-define(_PKIXCRMF_HRL_, true).

-record('CertReqMsg', {
  certReq,
  popo = asn1_NOVALUE,
  regInfo = asn1_NOVALUE
}).

-record('CertReqMsg_regInfo_SEQOF', {
  type,
  value
}).

-record('CertRequest', {
  certReqId,
  certTemplate,
  controls = asn1_NOVALUE
}).

-record('CertTemplate', {
  version = asn1_NOVALUE,
  serialNumber = asn1_NOVALUE,
  signingAlg = asn1_NOVALUE,
  issuer = asn1_NOVALUE,
  validity = asn1_NOVALUE,
  subject = asn1_NOVALUE,
  publicKey = asn1_NOVALUE,
  issuerUID = asn1_NOVALUE,
  subjectUID = asn1_NOVALUE,
  extensions = asn1_NOVALUE
}).

-record('CertTemplate_signingAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('CertTemplate_extensions_SEQOF', {
  extnID,
  critical = asn1_DEFAULT,
  extnValue
}).

-record('OptionalValidity', {
  notBefore = asn1_NOVALUE,
  notAfter = asn1_NOVALUE
}).

-record('Controls_SEQOF', {
  type,
  value
}).

-record('POPOSigningKey', {
  poposkInput = asn1_NOVALUE,
  algorithmIdentifier,
  signature
}).

-record('POPOSigningKey_algorithmIdentifier', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('POPOSigningKeyInput', {
  authInfo,
  publicKey
}).

-record('PKMACValue', {
  algId,
  value
}).

-record('PKMACValue_algId', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-ifndef(_PBMParameter_).
-define(_PBMParameter_, true).

-record('PBMParameter', {
  salt,
  owf,
  iterationCount,
  mac
}).

-record('PBMParameter_owf', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('PBMParameter_mac', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-endif.  %% _PBMParameter_

-record('EncKeyWithID', {
  privateKey,
  identifier = asn1_NOVALUE
}).

%% Include public_key.hrl for this
%% -record('PrivateKeyInfo', {
%%   version,
%%   privateKeyAlgorithm,
%%   privateKey,
%%   attributes = asn1_NOVALUE
%% }).

%% -record('PrivateKeyInfo_privateKeyAlgorithm', {
%%   algorithm,
%%   parameters = asn1_NOVALUE
%% }).

-record('Attributes_SETOF', {
  type,
  values
}).

-record('PKIPublicationInfo', {
  action,
  pubInfos = asn1_NOVALUE
}).

-record('SinglePubInfo', {
  pubMethod,
  pubLocation = asn1_NOVALUE
}).

-record('EncryptedValue', {
  intendedAlg = asn1_NOVALUE,
  symmAlg = asn1_NOVALUE,
  encSymmKey = asn1_NOVALUE,
  keyAlg = asn1_NOVALUE,
  valueHint = asn1_NOVALUE,
  encValue
}).

-record('EncryptedValue_intendedAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('EncryptedValue_symmAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('EncryptedValue_keyAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('CertId', {
  issuer,
  serialNumber
}).

-define('id-pkip', {1,3,6,1,5,5,7,5}).
-define('id-smime', {1,2,840,113549,1,9,16}).
-define('id-ct', {1,2,840,113549,1,9,16,1}).
%% Include public_key.hrl for this
%%-define('id-PasswordBasedMac', {1,2,840,113533,7,66,13}).
-define('id-ct-encKeyWithID', {1,2,840,113549,1,9,16,1,21}).
-define('id-regCtrl', {1,3,6,1,5,5,7,5,1}).
-define('id-regCtrl-regToken', {1,3,6,1,5,5,7,5,1,1}).
-define('id-regCtrl-authenticator', {1,3,6,1,5,5,7,5,1,2}).
-define('id-regCtrl-pkiPublicationInfo', {1,3,6,1,5,5,7,5,1,3}).
-define('id-regCtrl-pkiArchiveOptions', {1,3,6,1,5,5,7,5,1,4}).
-define('id-regCtrl-oldCertID', {1,3,6,1,5,5,7,5,1,5}).
-define('id-regCtrl-protocolEncrKey', {1,3,6,1,5,5,7,5,1,6}).
-define('id-regInfo', {1,3,6,1,5,5,7,5,2}).
-define('id-regInfo-utf8Pairs', {1,3,6,1,5,5,7,5,2,1}).
-define('id-regInfo-certReq', {1,3,6,1,5,5,7,5,2,2}).
-endif. %% _PKIXCRMF_HRL_
